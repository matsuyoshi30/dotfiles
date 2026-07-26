import json
import subprocess
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "parse_diff.py"
FIXTURE = Path(__file__).resolve().parent / "fixtures" / "sample.diff"


def run_parser(diff_text):
    with tempfile.TemporaryDirectory() as d:
        out = Path(d) / "hunks.json"
        proc = subprocess.run(
            ["python3", str(SCRIPT), str(out)],
            input=diff_text, capture_output=True, text=True,
        )
        assert proc.returncode == 0, proc.stderr
        return json.loads(out.read_text()), proc.stdout


class TestParseDiff(unittest.TestCase):
    def setUp(self):
        self.data, self.annotated = run_parser(FIXTURE.read_text())

    def test_hunk_count_and_ids(self):
        hunks = self.data["hunks"]
        self.assertEqual(len(hunks), 2)
        self.assertEqual([h["id"] for h in hunks], ["h001", "h002"])

    def test_file_attribution(self):
        self.assertEqual(self.data["hunks"][0]["file"], "src/foo.ts")
        self.assertEqual(self.data["hunks"][1]["file"], "src/bar.ts")

    def test_line_classification(self):
        types = [l["type"] for l in self.data["hunks"][0]["lines"]]
        self.assertEqual(types, ["context", "del", "add", "add", "context"])

    def test_stats(self):
        self.assertEqual(self.data["stats"],
                         {"files": 2, "files_total": 2, "no_hunk_files": [],
                          "hunks": 2, "added": 3, "removed": 2})

    def test_annotated_diff_has_ids(self):
        self.assertIn("[h001] src/foo.ts", self.annotated)
        self.assertIn("[h002] src/bar.ts", self.annotated)

    def test_context_lines_are_marked_ctx_not_a_bare_space(self):
        # The reported bug: with -U15 a hunk is mostly context, and the unified
        # format's bare leading space is too weak a signal — the reviewer read
        # unchanged code as part of the change. Context must be visibly "ctx".
        self.assertRegex(self.annotated, r"(?m)^ctx\d+\s*\| ")
        self.assertRegex(self.annotated, r"(?m)^\+\d+\s*\| ")
        self.assertRegex(self.annotated, r"(?m)^-\d+\s*\| ")

    def test_line_numbers_track_both_sides_independently(self):
        # A del line carries its OLD-side number, add/context carry the NEW-side
        # one. A single counter passes on simple hunks and drifts as soon as adds
        # and dels interleave, silently mis-numbering every later line.
        diff_text = (
            "diff --git a/app.py b/app.py\n"
            "index 1111111..2222222 100644\n"
            "--- a/app.py\n"
            "+++ b/app.py\n"
            "@@ -10,5 +20,6 @@\n"
            " keep_a\n"
            "-drop_a\n"
            "+add_a\n"
            "+add_b\n"
            " keep_b\n"
            "-drop_b\n"
            " keep_c\n"
        )
        data, annotated = run_parser(diff_text)
        got = [(l["type"], l["no"]) for l in data["hunks"][0]["lines"]]
        self.assertEqual(got, [
            ("context", 20),  # old 10 / new 20
            ("del", 11),      # old side only
            ("add", 21),      # new side only
            ("add", 22),
            ("context", 23),  # old 12 / new 23
            ("del", 13),
            ("context", 24),  # old 14 / new 24
        ])
        # The header summary must agree with the body, or it becomes a second
        # source of truth the reviewer can be misled by.
        self.assertIn("added(new): 21-22 | deleted(old): 11,13", annotated)

    def test_header_summary_says_none_when_a_side_is_empty(self):
        # An add-only hunk must say "deleted(old): none" rather than printing an
        # empty field the reviewer could read as a truncated list.
        diff_text = (
            "diff --git a/a.py b/a.py\n"
            "index 1111111..2222222 100644\n"
            "--- a/a.py\n"
            "+++ b/a.py\n"
            "@@ -1,1 +1,2 @@\n"
            " keep\n"
            "+added\n"
        )
        _, annotated = run_parser(diff_text)
        self.assertIn("added(new): 2 | deleted(old): none", annotated)

    def test_dash_prefixed_content_not_treated_as_metadata(self):
        # Bug 1 regression: a deleted "-- comment" line gets a "-" del prefix
        # and becomes "--- comment", colliding with the "--- " metadata check.
        # Symmetrically for an added "++ foo" line colliding with "+++ ".
        diff_text = (
            "diff --git a/query.sql b/query.sql\n"
            "index 1111111..2222222 100644\n"
            "--- a/query.sql\n"
            "+++ b/query.sql\n"
            "@@ -1,3 +1,3 @@\n"
            " SELECT 1;\n"
            "--- a SQL comment\n"
            "+++ something\n"
            " SELECT 2;\n"
        )
        data, _ = run_parser(diff_text)
        hunks = data["hunks"]
        self.assertEqual(len(hunks), 1)
        types = [l["type"] for l in hunks[0]["lines"]]
        self.assertEqual(types, ["context", "del", "add", "context"])
        contents = [l["content"] for l in hunks[0]["lines"]]
        self.assertEqual(
            contents, ["SELECT 1;", "-- a SQL comment", "++ something", "SELECT 2;"]
        )
        self.assertEqual(data["stats"]["added"], 1)
        self.assertEqual(data["stats"]["removed"], 1)

    def test_pure_rename_with_no_hunk_does_not_leak_into_previous_file(self):
        # Bug 2 regression: a later file with no "@@" hunk (pure rename) must not
        # have its preamble lines (similarity index / rename from / rename to)
        # appended as bogus trailing context lines onto the previous file's hunk.
        diff_text = (
            "diff --git a/src/foo.ts b/src/foo.ts\n"
            "index 1111111..2222222 100644\n"
            "--- a/src/foo.ts\n"
            "+++ b/src/foo.ts\n"
            "@@ -1,2 +1,2 @@\n"
            " import x from 'y';\n"
            "-const a = 1;\n"
            "+const a = 2;\n"
            "diff --git a/src/old.ts b/src/new.ts\n"
            "similarity index 100%\n"
            "rename from src/old.ts\n"
            "rename to src/new.ts\n"
        )
        data, _ = run_parser(diff_text)
        hunks = data["hunks"]
        self.assertEqual(len(hunks), 1)
        types = [l["type"] for l in hunks[0]["lines"]]
        self.assertEqual(types, ["context", "del", "add"])
        self.assertEqual(data["stats"]["hunks"], 1)
        # The hunk-less rename must still be visible in stats, or the screen
        # silently undercounts what changed.
        self.assertEqual(data["stats"]["files_total"], 2)
        self.assertEqual(data["stats"]["no_hunk_files"], ["src/new.ts"])

    def test_mnemonic_prefixes_are_stripped(self):
        # Regression: with diff.mnemonicPrefix set, `git diff HEAD` emits c/ (commit)
        # and w/ (worktree) prefixes instead of a/ b/. The parser must strip them so
        # "file" is a real path, not "w/config.py" (which breaks existing-file reads
        # and points HTML locations at nonexistent paths).
        diff_text = (
            "diff --git c/config.py w/config.py\n"
            "index b72501b..67f285b 100644\n"
            "--- c/config.py\n"
            "+++ w/config.py\n"
            "@@ -1,1 +1,1 @@\n"
            "-CACHE_ENABLED = False\n"
            "+CACHE_ENABLED = True\n"
        )
        data, annotated = run_parser(diff_text)
        self.assertEqual(data["hunks"][0]["file"], "config.py")
        self.assertIn("[h001] config.py", annotated)

    def test_exotic_line_separators_cannot_forge_diff_structure(self):
        # str.splitlines() also splits on \f, \v, \x85, U+2028/U+2029 and lone
        # \r INSIDE a source line, letting file content forge fake deletions or
        # hunk headers. The parser must split on "\n" only.
        for sep in ["\x0c", "\x0b", "\x85", " ", " ", "\r"]:
            diff_text = (
                "diff --git a/app.py b/app.py\n"
                "index 1111111..2222222 100644\n"
                "--- a/app.py\n"
                "+++ b/app.py\n"
                "@@ -1,2 +1,3 @@\n"
                " keep_a\n"
                f"+added {sep}- forged deletion\n"
                " keep_b\n"
            )
            data, _ = run_parser(diff_text)
            with self.subTest(sep=repr(sep)):
                self.assertEqual(data["stats"]["added"], 1)
                self.assertEqual(data["stats"]["removed"], 0)
                types = [l["type"] for l in data["hunks"][0]["lines"]]
                self.assertEqual(types, ["context", "add", "context"])

    def test_quoted_and_spaced_paths_resolve_to_real_files(self):
        # core.quotePath C-quotes non-ASCII names (octal per byte) and git adds
        # a trailing tab after names containing spaces; both must come out as
        # real paths or the blind reviewer's Read of the file always fails.
        quoted = '"w/\\346\\227\\245\\346\\234\\254\\350\\252\\236.py"'  # 日本語.py
        diff_text = (
            'diff --git "c/\\346\\227\\245\\346\\234\\254\\350\\252\\236.py" ' + quoted + "\n"
            "index 1111111..2222222 100644\n"
            '--- "c/\\346\\227\\245\\346\\234\\254\\350\\252\\236.py"\n'
            "+++ " + quoted + "\n"
            "@@ -1,1 +1,1 @@\n"
            "-a\n"
            "+b\n"
            "diff --git c/has space.py w/has space.py\n"
            "index 1111111..2222222 100644\n"
            "--- c/has space.py\t\n"
            "+++ w/has space.py\t\n"
            "@@ -1,1 +1,1 @@\n"
            "-x\n"
            "+y\n"
        )
        data, _ = run_parser(diff_text)
        self.assertEqual(data["hunks"][0]["file"], "日本語.py")
        self.assertEqual(data["hunks"][1]["file"], "has space.py")

    def test_combined_diff_fails_loudly(self):
        # Combined diffs (-c/--cc, "@@@" hunks) are unsupported; they must not
        # be silently dropped as 0 hunks with exit 0.
        diff_text = (
            "diff --combined app.py\n"
            "index 1111,2222..3333\n"
            "--- a/app.py\n"
            "+++ b/app.py\n"
            "@@@ -1,2 -1,2 +1,2 @@@\n"
            "  ctx\n"
            "++resolved\n"
        )
        with tempfile.TemporaryDirectory() as d:
            out = Path(d) / "hunks.json"
            proc = subprocess.run(
                ["python3", str(SCRIPT), str(out)],
                input=diff_text, capture_output=True, text=True,
            )
        self.assertNotEqual(proc.returncode, 0)
        self.assertIn("combined diff", proc.stderr)

    def test_deleted_file_uses_old_path_not_dev_null(self):
        # Bug 3 regression: a deleted file's "+++" line is "/dev/null"; the
        # hunk's file must come from the "--- a/old.txt" line instead.
        diff_text = (
            "diff --git a/old.txt b/old.txt\n"
            "index 1111111..0000000 100644\n"
            "--- a/old.txt\n"
            "+++ /dev/null\n"
            "@@ -1,2 +0,0 @@\n"
            "-line one\n"
            "-line two\n"
        )
        data, _ = run_parser(diff_text)
        hunks = data["hunks"]
        self.assertEqual(len(hunks), 1)
        self.assertEqual(hunks[0]["file"], "old.txt")


if __name__ == "__main__":
    unittest.main()
