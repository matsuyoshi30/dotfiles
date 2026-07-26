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


def run(hunks_doc, review_doc):
    with tempfile.TemporaryDirectory() as d:
        hp, rp, op = Path(d) / "h.json", Path(d) / "r.json", Path(d) / "o.html"
        hp.write_text(json.dumps(hunks_doc))
        rp.write_text(json.dumps(review_doc))
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
