import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "prepare_target.sh"


class TestPrepareTargetLocal(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = Path(self.tmp.name)
        self.env = {
            **os.environ,
            "GIT_CONFIG_GLOBAL": "/dev/null",
            "GIT_CONFIG_NOSYSTEM": "1",
            "GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@example.com",
            "GIT_COMMITTER_NAME": "t", "GIT_COMMITTER_EMAIL": "t@example.com",
        }
        self.git("init", "-q", "--bare", str(root / "remote.git"), cwd=root)
        self.work = root / "work"
        self.git("clone", "-q", str(root / "remote.git"), str(self.work), cwd=root)
        self.git("config", "diff.mnemonicPrefix", "true")
        (self.work / "keep.txt").write_text("a\nb\nc\n")
        (self.work / "old.txt").write_text("moved\n")
        self.git("add", ".")
        self.git("commit", "-qm", "init")
        self.git("push", "-q", "origin", "HEAD:main")
        self.git("remote", "set-head", "origin", "main")
        self.git("switch", "-qc", "feat")
        self.out = root / "out"

    def tearDown(self):
        self.tmp.cleanup()

    def git(self, *args, cwd=None):
        subprocess.run(["git", *args], cwd=cwd or self.work, env=self.env, check=True, capture_output=True)

    def prepare(self, *args):
        proc = subprocess.run(
            [str(SCRIPT), str(self.out), "local", *args],
            cwd=self.work, env=self.env, capture_output=True, text=True,
        )
        self.assertEqual(proc.returncode, 0, proc.stderr)
        hunks = json.loads((self.out / "hunks.json").read_text())
        return hunks, (self.out / "diff.txt").read_text(), proc.stderr

    def test_includes_branch_commits_and_their_messages(self):
        (self.work / "keep.txt").write_text("a\nB\nc\n")
        self.git("commit", "-qam", "change b")
        _, diff, _ = self.prepare()
        self.assertIn("+2      | B", diff)
        self.assertIn("change b", (self.out / "intent.md").read_text())

    def test_pure_rename_is_reported_without_hunks(self):
        self.git("mv", "old.txt", "new.txt")
        hunks, _, _ = self.prepare()
        self.assertEqual(hunks["stats"]["no_hunk_files"], ["new.txt"])

    def test_untracked_non_ascii_file_is_included(self):
        (self.work / "設計.md").write_text("new\n")
        _, diff, _ = self.prepare()
        self.assertIn("設計.md", diff)
        self.assertIn("+1      | new", diff)

    def test_named_unchanged_file_is_reviewed_whole(self):
        _, diff, _ = self.prepare("keep.txt")
        self.assertIn("+3      | c", diff)

    def test_named_path_narrows_the_diff(self):
        (self.work / "keep.txt").write_text("a\nB\nc\n")
        (self.work / "other.txt").write_text("x\n")
        _, diff, _ = self.prepare("keep.txt")
        self.assertNotIn("other.txt", diff)

    def test_warns_when_origin_head_is_missing(self):
        self.git("remote", "set-head", "origin", "--delete")
        (self.work / "keep.txt").write_text("a\nB\nc\n")
        _, diff, stderr = self.prepare()
        self.assertIn("origin/HEAD is not set", stderr)
        self.assertIn("+2      | B", diff)


if __name__ == "__main__":
    unittest.main()
