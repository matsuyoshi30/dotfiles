/**
 * Asks before the model runs a destructive bash command. pi ships without
 * a permission system, so bash is otherwise unchecked. The confirmation
 * names the matched rule; without a UI (print/RPC mode) matching commands
 * are refused outright.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

interface Rule {
	name: string;
	pattern: RegExp;
}

const rules: Rule[] = [
	{ name: "recursive force rm", pattern: /\brm\s+-[a-z]*(r[a-z]*f|f[a-z]*r)/i },
	{ name: "sudo", pattern: /\bsudo\b/ },
	{ name: "world-writable chmod/chown", pattern: /\b(chmod|chown)\b.*777/ },
	// --force-with-lease is deliberately not matched
	{ name: "git force push", pattern: /\bgit\s+push\b.*(\s--force(?!-with-lease)\b|\s-f\b)/ },
	{ name: "git hard reset", pattern: /\bgit\s+reset\s+--hard\b/ },
	{ name: "git clean", pattern: /\bgit\s+clean\s+-[a-z]*f/ },
];

export default function (pi: ExtensionAPI) {
	pi.on("tool_call", async (event, ctx) => {
		if (event.toolName !== "bash") return undefined;

		const command = event.input.command as string;
		const rule = rules.find((r) => r.pattern.test(command));
		if (!rule) return undefined;

		if (!ctx.hasUI) {
			return { block: true, reason: `Refused (${rule.name}): no UI to ask for confirmation` };
		}

		const allowed = await ctx.ui.confirm(`Run this command? (${rule.name})`, command);
		return allowed ? undefined : { block: true, reason: `User refused (${rule.name})` };
	});
}
