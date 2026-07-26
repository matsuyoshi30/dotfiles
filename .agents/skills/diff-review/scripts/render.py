#!/usr/bin/env python3
"""Turn hunks.json and review.json into a self-contained HTML file. Stdlib only."""
import argparse
import json
import re
import sys

RISK_ORDER = {"attention": 0, "low": 1, "safe": 2}
# Every finding key the template renders; a missing/non-string one used to kill
# render() mid-flight and leave a header-only "0 findings" screen that looks
# like a clean review. Validate here and warn (stderr is the skill's gate).
REQUIRED_FINDING_KEYS = ("id", "severity", "summary", "location", "detail")
# Replace the template's `= /*__REVIEW_DATA__*/ null;` with `= {json};`.
# Including the trailing ` null` in the token means no stray ` null` is left
# after substitution. The un-substituted template alone still opens as valid JS
# (`= null;`).
TOKEN = "/*__REVIEW_DATA__*/ null"


def load_json_lenient(f):
    """json.load, but tolerate the file being pasted LLM output.

    Accepts the raw JSON, or text containing ```json / ``` fenced blocks.
    A reply may hold several fences (prose examples before the real review),
    so try json-tagged blocks last-first, then untagged ones, then the whole
    text — first candidate that parses wins.
    """
    text = f.read()
    json_blocks = re.findall(r"^```json\s*$(.*?)^```\s*$", text, re.DOTALL | re.MULTILINE)
    any_blocks = re.findall(r"^```\w*\s*$(.*?)^```\s*$", text, re.DOTALL | re.MULTILINE)
    for cand in list(reversed(json_blocks)) + list(reversed(any_blocks)):
        try:
            return json.loads(cand)
        except json.JSONDecodeError:
            continue
    return json.loads(text)


def build_review_data(hunks_doc, review_doc):
    by_id = {h["id"]: h for h in hunks_doc["hunks"]}
    groups = []
    covered = set()
    unknown = []
    warnings = []
    seen_gids = set()
    for g in review_doc.get("groups", []):
        gid = g.get("id")
        if gid in seen_gids:
            warnings.append(f"duplicate group id {gid!r}")
        seen_gids.add(gid)
        if g.get("risk") not in RISK_ORDER:
            # An unrecognized risk would silently sort last and render unstyled —
            # the highest-risk group ending up least visible.
            warnings.append(f"group {gid!r}: unknown risk {g.get('risk')!r}")
        if not isinstance(g.get("findings"), list):
            warnings.append(f"group {gid!r}: findings[] missing or not a list")
            g = {**g, "findings": []}
        for f in g["findings"]:
            bad = [k for k in REQUIRED_FINDING_KEYS if not isinstance(f.get(k), str)]
            if bad:
                warnings.append(
                    f"group {gid!r} finding {f.get('id')!r}: missing/non-string keys: {', '.join(bad)}")
        resolved = []
        for hid in g.get("hunk_ids", []):
            if hid in by_id:
                if hid in covered:
                    warnings.append(f"hunk {hid} assigned to more than one group")
                resolved.append(by_id[hid])
                covered.add(hid)
            else:
                unknown.append(hid)
        groups.append({**g, "hunks": resolved})
    groups.sort(key=lambda g: RISK_ORDER.get(g.get("risk"), 99))
    # Detect review gaps. A hunk in no group means the human finishes review
    # without seeing part of the diff; a reference to a nonexistent id is the same
    # signal. Keep rendering (surface the gap instead of swallowing it).
    missing = [hid for hid in by_id if hid not in covered]
    if missing:
        warnings.append(f"{len(missing)} hunk(s) in no group: {', '.join(missing)}")
    if unknown:
        warnings.append(f"group referenced unknown hunk id(s): {', '.join(unknown)}")
    for w in warnings:
        print(f"warning: {w}", file=sys.stderr)
    return {
        "meta": {
            "stats": hunks_doc["stats"],
            "plan_checked": review_doc.get("plan_checked", False),
            "plan_path": review_doc.get("plan_path"),
        },
        "groups": groups,
    }


def render(hunks_doc, review_doc, template):
    data = build_review_data(hunks_doc, review_doc)
    # Escape every "<" (valid JSON string escape). Escaping only "</" is not
    # enough: an unpaired "<!--" or "<script" inside diff/finding text switches
    # the HTML parser into (double-)escaped script state, the real </script>
    # never closes the element, and the whole review renders as a blank page.
    payload = json.dumps(data, ensure_ascii=False).replace("<", "\\u003c")
    return template.replace(TOKEN, payload, 1)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--hunks", required=True)
    ap.add_argument("--review", required=True)
    ap.add_argument("--template", required=True)
    ap.add_argument("--out", required=True)
    args = ap.parse_args()
    with open(args.hunks, encoding="utf-8") as f:
        hunks_doc = json.load(f)
    with open(args.review, encoding="utf-8") as f:
        review_doc = load_json_lenient(f)
    with open(args.template, encoding="utf-8") as f:
        template = f.read()
    with open(args.out, "w", encoding="utf-8") as f:
        f.write(render(hunks_doc, review_doc, template))


if __name__ == "__main__":
    main()
