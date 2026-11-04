import json
import os
import subprocess
import tempfile
import time
import unittest
from pathlib import Path

HOOK = Path(__file__).resolve().parents[1] / "herdr-pr-token.sh"

# `gh pr view <number> --repo <repo> --json state --jq .state` answers from GH_PR_STATES ({"repo#number": state}).
GH_STUB = """#!/bin/bash
key="$5#$3"
printf '%s' "$GH_PR_STATES" | jq -er --arg k "$key" '.[$k]' || { echo 'not found' >&2; exit 1; }
"""

HERDR_STUB = """#!/bin/bash
echo "$*" >> "$HERDR_LOG"
"""


def pr_link(repo, number):
    return {"type": "pr-link", "prRepository": repo, "prNumber": number}


class HerdrPrTokenTest(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp())
        bin_dir = self.tmp / "bin"
        bin_dir.mkdir()
        for name, body in (("gh", GH_STUB), ("herdr", HERDR_STUB)):
            stub = bin_dir / name
            stub.write_text(body)
            stub.chmod(0o755)
        self.log = self.tmp / "herdr.log"
        self.transcript = self.tmp / "session.jsonl"
        self.env = {
            **os.environ,
            "PATH": f"{bin_dir}:{os.environ['PATH']}",
            "HERDR_LOG": str(self.log),
            "HERDR_ENV": "1",
            "HERDR_PANE_ID": "w1:p1",
        }

    def run_hook(self, entries, states, **env):
        lines = [{"type": "user", "message": "hi"}, *entries]
        self.transcript.write_text("".join(json.dumps(e, separators=(",", ":")) + "\n" for e in lines))
        proc = subprocess.run(
            ["bash", str(HOOK)],
            input=json.dumps({"transcript_path": str(self.transcript)}),
            capture_output=True,
            text=True,
            env={**self.env, "GH_PR_STATES": json.dumps(states), **env},
        )
        self.assertEqual(proc.returncode, 0, proc.stderr)

    def herdr_calls(self):
        deadline = time.time() + 5
        while time.time() < deadline and not self.log.exists():
            time.sleep(0.05)
        return self.log.read_text().splitlines() if self.log.exists() else []

    def test_open_prs_from_any_repo_are_shown_once_each(self):
        self.run_hook(
            [pr_link("o/backend", 10), pr_link("o/backend", 10), pr_link("o/web", 20)],
            {"o/backend#10": "OPEN", "o/web#20": "OPEN"},
        )
        self.assertEqual(
            self.herdr_calls(),
            ["pane report-metadata w1:p1 --source dotfiles:pr --token pr=#10 #20"],
        )

    def test_merged_and_closed_prs_are_dropped(self):
        self.run_hook(
            [pr_link("o/backend", 10), pr_link("o/web", 20), pr_link("o/web", 30)],
            {"o/backend#10": "MERGED", "o/web#20": "OPEN", "o/web#30": "CLOSED"},
        )
        self.assertEqual(
            self.herdr_calls(),
            ["pane report-metadata w1:p1 --source dotfiles:pr --token pr=#20"],
        )

    def test_no_open_pr_clears_token(self):
        self.run_hook([pr_link("o/backend", 10)], {"o/backend#10": "MERGED"})
        self.assertEqual(
            self.herdr_calls(),
            ["pane report-metadata w1:p1 --source dotfiles:pr --clear-token pr"],
        )

    def test_session_without_pr_clears_token(self):
        self.run_hook([], {})
        self.assertEqual(
            self.herdr_calls(),
            ["pane report-metadata w1:p1 --source dotfiles:pr --clear-token pr"],
        )

    def test_outside_herdr_does_nothing(self):
        self.run_hook([pr_link("o/backend", 10)], {"o/backend#10": "OPEN"}, HERDR_ENV="")
        time.sleep(0.5)
        self.assertFalse(self.log.exists())


if __name__ == "__main__":
    unittest.main()
