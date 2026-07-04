/**
 * Adds external directories to the session context: each dir's AGENTS.md
 * (or CLAUDE.md) is appended to the system prompt every turn, so the agent
 * stays aware of sibling projects. The dir list persists in the session as
 * custom entries and survives /resume and branching.
 *
 * Commands: /add-dir <path>, /remove-dir <path>, /dirs
 *
 * Self-written replacement for the community pi-add-dir package
 * (context files only; it does not load skills from added dirs).
 */

import { existsSync, readFileSync, statSync } from "node:fs";
import { homedir } from "node:os";
import { join, resolve } from "node:path";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

// AGENTS.md wins over CLAUDE.md, mirroring pi's own context-file loading
const CONTEXT_FILES = ["AGENTS.md", "CLAUDE.md"];

function contextFileIn(dir: string): string | undefined {
	for (const name of CONTEXT_FILES) {
		const file = join(dir, name);
		if (existsSync(file)) return file;
	}
	return undefined;
}

export default function (pi: ExtensionAPI) {
	let dirs: string[] = [];

	const reconstruct = (ctx: ExtensionContext) => {
		dirs = [];
		for (const entry of ctx.sessionManager.getBranch()) {
			if (entry.type === "custom" && entry.customType === "add-dir") {
				dirs = (entry.data as { dirs: string[] }).dirs;
			}
		}
	};
	pi.on("session_start", async (_event, ctx) => reconstruct(ctx));
	pi.on("session_tree", async (_event, ctx) => reconstruct(ctx));

	const expand = (p: string) => resolve(p.replace(/^~(?=\/|$)/, homedir()));

	pi.registerCommand("add-dir", {
		description: "Add an external directory's AGENTS.md/CLAUDE.md to context",
		handler: async (args, ctx) => {
			const raw = (args ?? "").trim();
			const path = raw ? expand(raw) : "";
			if (!path || !existsSync(path) || !statSync(path).isDirectory()) {
				ctx.ui.notify(`Not a directory: ${raw || "(missing argument)"}`, "error");
				return;
			}
			if (!dirs.includes(path)) {
				dirs = [...dirs, path];
				pi.appendEntry("add-dir", { dirs });
			}
			const file = contextFileIn(path);
			ctx.ui.notify(
				file ? `Added ${path} (${file.slice(path.length + 1)})` : `Added ${path} (no AGENTS.md/CLAUDE.md found)`,
				file ? "info" : "warning",
			);
		},
	});

	pi.registerCommand("remove-dir", {
		description: "Remove a directory added via /add-dir",
		handler: async (args, ctx) => {
			const path = expand((args ?? "").trim());
			if (!dirs.includes(path)) {
				ctx.ui.notify(`Not in list: ${path}`, "error");
				return;
			}
			dirs = dirs.filter((d) => d !== path);
			pi.appendEntry("add-dir", { dirs });
			ctx.ui.notify(`Removed ${path}`, "info");
		},
	});

	pi.registerCommand("dirs", {
		description: "List directories added via /add-dir",
		handler: async (_args, ctx) => {
			ctx.ui.notify(dirs.length ? dirs.join("\n") : "No directories added", "info");
		},
	});

	pi.on("before_agent_start", async (event) => {
		const sections: string[] = [];
		for (const dir of dirs) {
			const file = contextFileIn(dir);
			if (file) {
				sections.push(`# Context from ${file}\n\n${readFileSync(file, "utf8")}`);
			}
		}
		if (sections.length === 0) return undefined;
		return { systemPrompt: `${event.systemPrompt}\n\n${sections.join("\n\n")}` };
	});
}
