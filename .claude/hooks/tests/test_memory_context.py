import json
import os
import subprocess
import tempfile
import time
import unittest
from pathlib import Path

HOOK = Path(__file__).resolve().parents[1] / "memory-context.sh"
SESSION_A = "11111111-1111-1111-1111-111111111111"
SESSION_B = "22222222-2222-2222-2222-222222222222"


class MemoryContextTest(unittest.TestCase):
    def setUp(self):
        self.home = Path(tempfile.mkdtemp())
        self.short_term = self.home / ".matsuyoshi30" / "memory" / "short-term"
        self.short_term.mkdir(parents=True)

    def run_hook(self, event, session_id=SESSION_A):
        proc = subprocess.run(
            ["bash", str(HOOK)],
            input=json.dumps({"hook_event_name": event, "session_id": session_id}),
            capture_output=True,
            text=True,
            env={**os.environ, "HOME": str(self.home), "TMPDIR": str(self.home / "tmp")},
        )
        self.assertEqual(proc.returncode, 0, proc.stderr)
        if not proc.stdout:
            return None
        return json.loads(proc.stdout)["hookSpecificOutput"]["additionalContext"]

    def test_each_event_points_at_this_sessions_file_only(self):
        own = str(self.short_term / f"{SESSION_A}.md")
        for event in ("SessionStart", "UserPromptSubmit"):
            context = self.run_hook(event)
            self.assertIn(own, context)
            self.assertNotIn(SESSION_B, context)
            self.assertNotIn("current.md", context)

    def test_session_start_restores_only_when_own_file_exists(self):
        (self.short_term / f"{SESSION_B}.md").write_text("other session")
        self.assertNotIn("restore phase", self.run_hook("SessionStart"))

        (self.short_term / f"{SESSION_A}.md").write_text("own session")
        self.assertIn("restore phase", self.run_hook("SessionStart"))

    def test_session_start_prunes_only_other_stale_session_files(self):
        stale_other = self.short_term / f"{SESSION_B}.md"
        stale_own = self.short_term / f"{SESSION_A}.md"
        note = self.short_term / "design-notes.md"
        old = time.time() - 15 * 86400
        for path in (stale_other, stale_own, note):
            path.write_text("x")
            os.utime(path, (old, old))

        context = self.run_hook("SessionStart")

        self.assertFalse(stale_other.exists())
        self.assertTrue(stale_own.exists())
        self.assertTrue(note.exists())
        self.assertIn("restore phase", context)

    def test_prompt_context_only_on_first_prompt_until_next_session_start(self):
        self.assertIn("index.md", self.run_hook("UserPromptSubmit"))
        self.assertIsNone(self.run_hook("UserPromptSubmit"))
        self.assertIn("index.md", self.run_hook("UserPromptSubmit", session_id=SESSION_B))

        self.run_hook("SessionStart")
        self.assertIn("index.md", self.run_hook("UserPromptSubmit"))

    def test_unknown_event_or_missing_session_is_silent(self):
        self.assertIsNone(self.run_hook("PreCompact"))
        self.assertIsNone(self.run_hook("SessionStart", session_id=""))


if __name__ == "__main__":
    unittest.main()
