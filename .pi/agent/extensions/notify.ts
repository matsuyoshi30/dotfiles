/**
 * Plays a sound and sends a terminal notification when the agent finishes
 * its turn and waits for input. Sound matches the Claude Code Stop hook
 * (afplay Submarine.aiff); the OSC 777 notification is picked up by
 * Ghostty, WezTerm, and iTerm2.
 */

import { spawn } from "node:child_process";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const SOUND = "/System/Library/Sounds/Submarine.aiff";

export default function (pi: ExtensionAPI) {
	pi.on("agent_end", async (_event, ctx) => {
		// Skip in print/RPC mode: escape codes would pollute captured output.
		if (!ctx.hasUI) return;
		if (process.platform === "darwin") {
			spawn("afplay", [SOUND], { detached: true, stdio: "ignore" }).unref();
		}
		process.stdout.write("\x1b]777;notify;pi;Ready for input\x07");
	});
}
