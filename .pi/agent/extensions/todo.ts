/**
 * Session-scoped todo list. The model manages it via the `todo` tool;
 * /todos prints the current list. Every tool result carries the full list,
 * and state is rebuilt by replaying the current branch, so it stays
 * correct across /resume and /tree branching.
 *
 * Self-written replacement for the community @juicesharp/rpiv-todo package.
 */

import { StringEnum } from "@earendil-works/pi-ai";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";

interface TodoItem {
	id: number;
	text: string;
	done: boolean;
}

interface TodoState {
	todos: TodoItem[];
	nextId: number;
}

const Params = Type.Object({
	action: StringEnum(["add", "done", "remove", "clear", "list"] as const),
	text: Type.Optional(Type.String({ description: "Item text (add)" })),
	id: Type.Optional(Type.Number({ description: "Item id (done / remove)" })),
});

function formatList(todos: TodoItem[]): string {
	if (todos.length === 0) return "No todos";
	return todos.map((t) => `${t.done ? "[x]" : "[ ]"} #${t.id} ${t.text}`).join("\n");
}

export default function (pi: ExtensionAPI) {
	let state: TodoState = { todos: [], nextId: 1 };

	const rebuild = (ctx: ExtensionContext) => {
		state = { todos: [], nextId: 1 };
		for (const entry of ctx.sessionManager.getBranch()) {
			if (entry.type !== "message" || entry.message.role !== "toolResult") continue;
			if (entry.message.toolName !== "todo") continue;
			const details = entry.message.details as TodoState | undefined;
			if (details?.todos) state = { todos: [...details.todos], nextId: details.nextId };
		}
	};
	pi.on("session_start", async (_event, ctx) => rebuild(ctx));
	pi.on("session_tree", async (_event, ctx) => rebuild(ctx));

	pi.registerTool({
		name: "todo",
		label: "Todo",
		description: "Track a session todo list. Actions: add (text), done (id), remove (id), clear, list.",
		parameters: Params,

		async execute(_toolCallId, params, _signal, _onUpdate, _ctx) {
			let error: string | undefined;
			switch (params.action) {
				case "add":
					if (params.text?.trim()) {
						state.todos.push({ id: state.nextId++, text: params.text.trim(), done: false });
					} else {
						error = "text is required for add";
					}
					break;
				case "done": {
					const item = state.todos.find((t) => t.id === params.id);
					if (item) {
						item.done = true;
					} else {
						error = `no todo with id ${params.id}`;
					}
					break;
				}
				case "remove": {
					const before = state.todos.length;
					state.todos = state.todos.filter((t) => t.id !== params.id);
					if (state.todos.length === before) error = `no todo with id ${params.id}`;
					break;
				}
				case "clear":
					state = { todos: [], nextId: 1 };
					break;
				case "list":
					break;
			}

			return {
				content: [{ type: "text", text: error ? `Error: ${error}` : formatList(state.todos) }],
				// Full state in every result so rebuild() can replay the branch
				details: { todos: [...state.todos], nextId: state.nextId } satisfies TodoState,
			};
		},
	});

	pi.registerCommand("todos", {
		description: "Show the current todo list",
		handler: async (_args, ctx) => {
			ctx.ui.notify(formatList(state.todos), "info");
		},
	});
}
