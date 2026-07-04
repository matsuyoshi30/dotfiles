/**
 * Lets the model ask the user a structured question: pick one of the
 * offered options or type a free-form answer. Built on pi's stock select
 * and input dialogs rather than custom TUI rendering.
 *
 * Self-written replacement for the community
 * @juicesharp/rpiv-ask-user-question package.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";

const OTHER = "Other (type an answer)";

const Params = Type.Object({
	question: Type.String({ description: "Question to ask the user" }),
	options: Type.Array(Type.String(), { description: "Choices offered to the user" }),
});

export default function (pi: ExtensionAPI) {
	pi.registerTool({
		name: "question",
		label: "Question",
		description:
			"Ask the user a question with predefined options; the user can also type a custom answer. Use when you need a decision from the user to proceed.",
		parameters: Params,

		async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
			const respond = (text: string) => ({
				content: [{ type: "text" as const, text }],
				details: {},
			});

			if (!ctx.hasUI) return respond("Error: cannot ask the user in non-interactive mode");

			const choice = await ctx.ui.select(params.question, [...params.options, OTHER]);
			if (choice === undefined) return respond("User cancelled the question");
			if (choice !== OTHER) return respond(`User selected: ${choice}`);

			const answer = (await ctx.ui.input("Your answer:"))?.trim();
			return answer ? respond(`User answered: ${answer}`) : respond("User cancelled the question");
		},
	});
}
