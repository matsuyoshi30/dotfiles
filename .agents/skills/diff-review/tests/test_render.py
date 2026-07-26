import json
import subprocess
import tempfile
import unittest
from pathlib import Path

BASE = Path(__file__).resolve().parents[1]
SCRIPT = BASE / "scripts" / "render.py"
TEMPLATE = BASE / "scripts" / "template.html"

HUNKS = {
    "stats": {"files": 1, "hunks": 2, "added": 2, "removed": 1},
    "hunks": [
        {"id": "h001", "file": "src/foo.ts", "header": "@@ -1,2 +1,3 @@",
         "old_start": 1, "new_start": 1,
         "lines": [{"type": "del", "content": "old"}, {"type": "add", "content": "new"}]},
        {"id": "h002", "file": "src/foo.ts", "header": "@@ -9,1 +9,2 @@",
         "old_start": 9, "new_start": 9,
         "lines": [{"type": "add", "content": "extra"}]},
    ],
}
REVIEW = {
    "plan_checked": True, "plan_path": "plans/x.md",
    "groups": [
        {"id": "g1", "title": "Low-risk change", "intent": "minor", "risk": "safe",
         "hunk_ids": ["h002"], "findings": []},
        {"id": "g2", "title": "Attention-worthy core change", "intent": "core", "risk": "attention",
         "hunk_ids": ["h001"],
         "findings": [{"id": "f1", "severity": "warning", "summary": "possible misuse",
                       "location": "src/foo.ts:1", "detail": "details",
                       "source": "plan_crosscheck"}]},
    ],
}


def run(hunks_doc, review_doc, review_text=None):
    with tempfile.TemporaryDirectory() as d:
        hp, rp, op = Path(d) / "h.json", Path(d) / "r.json", Path(d) / "o.html"
        hp.write_text(json.dumps(hunks_doc))
        rp.write_text(review_text if review_text is not None else json.dumps(review_doc))
        proc = subprocess.run(
            ["python3", str(SCRIPT), "--hunks", str(hp), "--review", str(rp),
             "--template", str(TEMPLATE), "--out", str(op)],
            capture_output=True, text=True)
        html = op.read_text() if op.exists() else ""
        return proc, html


def render():
    proc, html = run(HUNKS, REVIEW)
    assert proc.returncode == 0, proc.stderr
    return html


class TestRender(unittest.TestCase):
    def setUp(self):
        self.html = render()

    def test_no_external_requests(self):
        low = self.html.lower()
        self.assertNotIn("http://", low)
        self.assertNotIn("https://", low)
        self.assertNotIn("src=\"//", low)

    def test_embeds_review_data(self):
        self.assertIn("window.REVIEW_DATA", self.html)
        self.assertIn("Attention-worthy core change", self.html)
        self.assertIn("possible misuse", self.html)

    def test_risk_order_attention_before_safe(self):
        # REVIEW_DATA.groups is sorted attention -> safe.
        i_attention = self.html.index("Attention-worthy core change")
        i_safe = self.html.index("Low-risk change")
        self.assertLess(i_attention, i_safe)

    def test_hunk_content_resolved(self):
        # hunk_ids resolve to their body lines and the diff content is embedded.
        self.assertIn("new", self.html)
        self.assertIn("extra", self.html)

    def test_source_badge_label_present(self):
        # A source: plan_crosscheck finding gets its badge label embedded.
        self.assertIn("added by plan cross-check", self.html)

    def test_accepts_fenced_review_json(self):
        # A review.json saved with the LLM's ```json fence markers still loads.
        fenced = "```json\n" + json.dumps(REVIEW) + "\n```\n"
        proc, html = run(HUNKS, None, review_text=fenced)
        self.assertEqual(proc.returncode, 0, proc.stderr)
        self.assertIn("Attention-worthy core change", html)

    def test_prefers_parseable_fence_over_earlier_prose_fence(self):
        # An LLM reply may hold a prose example fence before the real review;
        # the loader must not blindly grab the first fence.
        text = ("Notes first:\n```\njust prose, not json\n```\n"
                "The review:\n```json\n" + json.dumps(REVIEW) + "\n```\n")
        proc, html = run(HUNKS, None, review_text=text)
        self.assertEqual(proc.returncode, 0, proc.stderr)
        self.assertIn("Attention-worthy core change", html)

    def test_all_angle_brackets_escaped_in_payload(self):
        # An unpaired "<!--" or "<script" in diff/finding text puts the HTML
        # parser into (double-)escaped script state and the page renders blank.
        # Every "<" in the embedded JSON must be escaped, not just "</".
        hunks = json.loads(json.dumps(HUNKS))
        hunks["hunks"][0]["lines"][0]["content"] = "<!-- opening comment <script"
        review = json.loads(json.dumps(REVIEW))
        review["groups"][1]["findings"][0]["detail"] = "the `<!--` here is never closed"
        proc, html = run(hunks, review)
        self.assertEqual(proc.returncode, 0, proc.stderr)
        start = html.index("window.REVIEW_DATA")
        end = html.index("</script>", start)
        payload = html[start:end]
        self.assertNotIn("<!--", payload)
        self.assertNotIn("<script", payload)
        self.assertIn("\\u003c!--", payload)

    def test_warns_on_duplicate_hunk_assignment(self):
        review = json.loads(json.dumps(REVIEW))
        review["groups"][0]["hunk_ids"] = ["h001", "h002"]  # h001 also in g2
        proc, _ = run(HUNKS, review)
        self.assertEqual(proc.returncode, 0)
        self.assertIn("more than one group", proc.stderr)

    def test_warns_on_unknown_risk_and_duplicate_group_id(self):
        review = json.loads(json.dumps(REVIEW))
        review["groups"][0]["risk"] = "atention"  # typo
        review["groups"][1]["id"] = review["groups"][0]["id"]
        proc, _ = run(HUNKS, review)
        self.assertEqual(proc.returncode, 0)
        self.assertIn("unknown risk", proc.stderr)
        self.assertIn("duplicate group id", proc.stderr)

    def test_warns_on_missing_findings_and_finding_keys(self):
        # A group without findings[] (or a finding missing summary) used to
        # abort render() client-side, leaving a header-only page that looks
        # like a clean "no findings" review — with exit 0 and empty stderr.
        review = json.loads(json.dumps(REVIEW))
        del review["groups"][0]["findings"]
        del review["groups"][1]["findings"][0]["summary"]
        proc, html = run(HUNKS, review)
        self.assertEqual(proc.returncode, 0)
        self.assertIn("findings[] missing", proc.stderr)
        self.assertIn("missing/non-string keys: summary", proc.stderr)
        self.assertIn("window.REVIEW_DATA", html)  # still renders; gate catches it

    def test_warns_on_uncovered_hunk(self):
        # Leaving h002 out of every group emits a stderr warning about the gap.
        review = {"plan_checked": False, "plan_path": None, "groups": [
            {"id": "g1", "title": "partial", "intent": "x", "risk": "safe",
             "hunk_ids": ["h001"], "findings": []},
        ]}
        proc, _ = run(HUNKS, review)
        self.assertEqual(proc.returncode, 0)  # warn only, don't fail
        self.assertIn("h002", proc.stderr)
        self.assertIn("warning", proc.stderr)


if __name__ == "__main__":
    unittest.main()
