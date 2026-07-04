/**
 * Refuses write/edit tool calls that touch secrets, VCS internals, or
 * dependency directories. Matches whole path segments, so names that
 * merely contain a protected string (.gitignore, config.envelope.ts)
 * pass through.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const PROTECTED_SEGMENTS = new Set([".git", ".ssh", "node_modules"]);
const isEnvFile = (seg: string) => seg === ".env" || seg.startsWith(".env.");

function offendingSegment(path: string): string | undefined {
	return path.split("/").find((seg) => PROTECTED_SEGMENTS.has(seg) || isEnvFile(seg));
}

export default function (pi: ExtensionAPI) {
	pi.on("tool_call", async (event, ctx) => {
		if (event.toolName !== "write" && event.toolName !== "edit") return undefined;

		const path = event.input.path as string;
		const segment = offendingSegment(path);
		if (!segment) return undefined;

		if (ctx.hasUI) {
			ctx.ui.notify(`Refused ${event.toolName} to ${path} (protected: ${segment})`, "warning");
		}
		return { block: true, reason: `"${path}" is under protected segment "${segment}"` };
	});
}
