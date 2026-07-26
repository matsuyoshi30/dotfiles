#!/usr/bin/env python3
"""Turn hunks.json and review.json into a self-contained HTML file. Stdlib only."""
import argparse
import json
import sys

RISK_ORDER = {"attention": 0, "low": 1, "safe": 2}
# Replace the template's `= /*__REVIEW_DATA__*/ null;` with `= {json};`.
# Including the trailing ` null` in the token means no stray ` null` is left
# after substitution. The un-substituted template alone still opens as valid JS
# (`= null;`).
TOKEN = "/*__REVIEW_DATA__*/ null"


def build_review_data(hunks_doc, review_doc):
    by_id = {h["id"]: h for h in hunks_doc["hunks"]}
    groups = []
    covered = set()
    unknown = []
    for g in review_doc["groups"]:
        resolved = []
        for hid in g.get("hunk_ids", []):
            if hid in by_id:
                resolved.append(by_id[hid])
                covered.add(hid)
            else:
                unknown.append(hid)
        groups.append({**g, "hunks": resolved})
    groups.sort(key=lambda g: RISK_ORDER.get(g["risk"], 99))
    # Detect review gaps. A hunk in no group means the human finishes review
    # without seeing part of the diff; a reference to a nonexistent id is the same
    # signal. Keep rendering (surface the gap instead of swallowing it).
    missing = [hid for hid in by_id if hid not in covered]
    if missing:
        print(f"warning: {len(missing)} hunk(s) in no group: {', '.join(missing)}", file=sys.stderr)
    if unknown:
        print(f"warning: group referenced unknown hunk id(s): {', '.join(unknown)}", file=sys.stderr)
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
    # Escape so a stray </script> can't break the HTML.
    payload = json.dumps(data, ensure_ascii=False).replace("</", "<\\/")
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
        review_doc = json.load(f)
    with open(args.template, encoding="utf-8") as f:
        template = f.read()
    with open(args.out, "w", encoding="utf-8") as f:
        f.write(render(hunks_doc, review_doc, template))


if __name__ == "__main__":
    main()
