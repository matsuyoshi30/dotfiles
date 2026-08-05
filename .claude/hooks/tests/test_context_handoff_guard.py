import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path

HOOK = Path(__file__).resolve().parents[1] / "context-handoff-guard.sh"
SESSION_ID = "11111111-2222-3333-4444-555555555555"


class GuardTestCase(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.tmpdir = Path(self._tmp.name)
        self.state_dir = self.tmpdir / "claude-ctx"
        self.state_dir.mkdir()
        self.addCleanup(self._tmp.cleanup)

    def write_state(self, pct):
        (self.state_dir / f"{SESSION_ID}.json").write_text(
            json.dumps({"pct": pct, "size": 1000000})
        )

    @property
    def fired_marker(self):
        return self.state_dir / f"{SESSION_ID}.fired"

    def run_hook(self, session_id=SESSION_ID, **env):
        proc = subprocess.run(
            ["bash", str(HOOK)],
            input=json.dumps({"session_id": session_id, "hook_event_name": "Stop"}),
            capture_output=True,
            text=True,
            env={**os.environ, "TMPDIR": str(self.tmpdir), **env},
        )
        self.assertEqual(proc.returncode, 0, proc.stderr)
        return proc.stdout.strip()

    def assert_fires(self, out):
        payload = json.loads(out)
        # `block` is load-bearing: Stop's additionalContext was measured to be inert.
        self.assertEqual(payload["decision"], "block")
        return payload["reason"]

    def test_stays_silent_below_the_threshold(self):
        self.write_state(39)
        self.assertEqual(self.run_hook(CLAUDE_HANDOFF_PCT="40"), "")

    def test_fires_at_the_threshold(self):
        self.write_state(40)
        context = self.assert_fires(self.run_hook(CLAUDE_HANDOFF_PCT="40"))
        self.assertIn("40%", context)
        self.assertIn("session-handoff", context)

    def test_fires_only_once_per_rising_edge(self):
        self.write_state(45)
        self.assert_fires(self.run_hook(CLAUDE_HANDOFF_PCT="40"))
        self.assertEqual(self.run_hook(CLAUDE_HANDOFF_PCT="40"), "")

    def test_rearms_after_usage_drops(self):
        self.write_state(45)
        self.assert_fires(self.run_hook(CLAUDE_HANDOFF_PCT="40"))

        # A compaction lands and usage falls back, which clears the marker.
        self.write_state(12)
        self.assertEqual(self.run_hook(CLAUDE_HANDOFF_PCT="40"), "")
        self.assertFalse(self.fired_marker.exists())

        self.write_state(45)
        self.assert_fires(self.run_hook(CLAUDE_HANDOFF_PCT="40"))

    def test_silent_without_a_state_file(self):
        # Headless runs have no statusline, so there is nothing to judge on.
        self.assertEqual(self.run_hook(CLAUDE_HANDOFF_PCT="40"), "")

    def test_defaults_to_eightyfive(self):
        def run_bare(pct):
            self.write_state(pct)
            env = {k: v for k, v in os.environ.items() if k != "CLAUDE_HANDOFF_PCT"}
            proc = subprocess.run(
                ["bash", str(HOOK)],
                input=json.dumps({"session_id": SESSION_ID}),
                capture_output=True,
                text=True,
                env={**env, "TMPDIR": str(self.tmpdir)},
            )
            self.assertEqual(proc.returncode, 0, proc.stderr)
            return proc.stdout.strip()

        self.assertEqual(run_bare(84), "")
        self.assertIn("85%", self.assert_fires(run_bare(85)))

    def test_autocompact_override_no_longer_moves_the_threshold(self):
        # The env var was measured not to mean "compact at this percentage", so it
        # must not be mistaken for a baseline again.
        self.write_state(50)
        env = {k: v for k, v in os.environ.items() if k != "CLAUDE_HANDOFF_PCT"}
        proc = subprocess.run(
            ["bash", str(HOOK)],
            input=json.dumps({"session_id": SESSION_ID}),
            capture_output=True,
            text=True,
            env={**env, "TMPDIR": str(self.tmpdir), "CLAUDE_AUTOCOMPACT_PCT_OVERRIDE": "50"},
        )
        self.assertEqual(proc.stdout.strip(), "")

    def test_explicit_threshold_wins(self):
        self.write_state(30)
        self.assert_fires(self.run_hook(CLAUDE_HANDOFF_PCT="25"))

    def test_ignores_a_malformed_state_file(self):
        (self.state_dir / f"{SESSION_ID}.json").write_text("not json")
        self.assertEqual(self.run_hook(CLAUDE_HANDOFF_PCT="40"), "")


class StatuslineRelayTestCase(unittest.TestCase):
    """The statusline is the producer of the file the hook consumes."""

    STATUSLINE = Path(__file__).resolve().parents[1].parent / "statusline.py"

    def test_writes_the_state_file_the_hook_reads(self):
        with tempfile.TemporaryDirectory() as tmp:
            payload = {
                "session_id": SESSION_ID,
                "model": {"display_name": "Opus 5"},
                "workspace": {"current_dir": tmp},
                "context_window": {"used_percentage": 42, "context_window_size": 1000000},
            }
            proc = subprocess.run(
                ["python3", str(self.STATUSLINE)],
                input=json.dumps(payload),
                capture_output=True,
                text=True,
                env={**os.environ, "TMPDIR": tmp},
            )
            self.assertEqual(proc.returncode, 0, proc.stderr)
            self.assertIn("ctx 42%", proc.stdout)

            state = Path(tmp) / "claude-ctx" / f"{SESSION_ID}.json"
            self.assertEqual(json.loads(state.read_text())["pct"], 42)

    def test_survives_a_payload_without_context_window(self):
        with tempfile.TemporaryDirectory() as tmp:
            payload = {"model": {"display_name": "Opus 5"}, "workspace": {"current_dir": tmp}}
            proc = subprocess.run(
                ["python3", str(self.STATUSLINE)],
                input=json.dumps(payload),
                capture_output=True,
                text=True,
                env={**os.environ, "TMPDIR": tmp},
            )
            self.assertEqual(proc.returncode, 0, proc.stderr)
            self.assertIn("Opus 5", proc.stdout)


if __name__ == "__main__":
    unittest.main()
