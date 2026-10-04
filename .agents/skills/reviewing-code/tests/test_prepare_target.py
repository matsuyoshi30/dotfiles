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


FAKE_GH = r"""#!/usr/bin/env bash
filter= args=()
while [ $# -gt 0 ]; do
  case $1 in -q|--jq) filter=$2; shift 2 ;; *) args+=("$1"); shift ;; esac
done
case "${args[*]}" in
  "pr view 7 --json url") json='{"url":"https://github.com/o/r/pull/7"}' ;;
  "pr view 7 --json author,comments") json=$(cat "$FAKE_GH_DIR/comments.json") ;;
  "pr view 7") echo "title: Add foo"; exit ;;
  "pr diff 7") cat "$FAKE_GH_DIR/pr.diff"; exit ;;
  *) echo "unexpected gh call: ${args[*]}" >&2; exit 1 ;;
esac
jq -r "$filter" <<<"$json"
"""


class TestPrepareTargetPr(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = Path(self.tmp.name)
        fake = root / "bin"
        fake.mkdir()
        (fake / "gh").write_text(FAKE_GH)
        (fake / "gh").chmod(0o755)
        (fake / "pr.diff").write_text((Path(__file__).parent / "fixtures" / "sample.diff").read_text())
        (fake / "comments.json").write_text(json.dumps({
            "author": {"login": "alice"},
            "comments": [
                {"author": {"login": "alice"}, "body": "Spec: https://linear.app/x/issue/ABC-1"},
                {"author": {"login": "bob"}, "body": "drive-by remark"},
            ],
        }))
        self.env = {**os.environ, "PATH": f"{fake}:{os.environ['PATH']}", "FAKE_GH_DIR": str(fake),
                    "GIT_CONFIG_GLOBAL": "/dev/null", "GIT_CONFIG_NOSYSTEM": "1"}
        self.work = root / "work"
        subprocess.run(["git", "init", "-q", str(self.work)], env=self.env, check=True)
        subprocess.run(["git", "remote", "add", "origin", "https://github.com/o/r.git"],
                       cwd=self.work, env=self.env, check=True)
        self.out = root / "out"

    def tearDown(self):
        self.tmp.cleanup()

    def test_intent_includes_only_the_pr_authors_comments(self):
        proc = subprocess.run([str(SCRIPT), str(self.out), "pr", "7"],
                              cwd=self.work, env=self.env, capture_output=True, text=True)
        self.assertEqual(proc.returncode, 0, proc.stderr)
        intent = (self.out / "intent.md").read_text()
        self.assertIn("title: Add foo", intent)
        self.assertIn("https://linear.app/x/issue/ABC-1", intent)
        self.assertNotIn("drive-by remark", intent)


if __name__ == "__main__":
    unittest.main()
