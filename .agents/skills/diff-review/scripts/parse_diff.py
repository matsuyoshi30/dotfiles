#!/usr/bin/env python3
"""Convert a raw unified diff into hunks.json and an annotated diff text.

Reads a diff from stdin, writes hunks.json to the path given as argv[1], and
emits the annotated diff to stdout. Depends only on the standard library.
"""
import json
import re
import sys

HUNK_HEADER = re.compile(r"^@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)? @@(.*)$")

# Prefixes git puts before paths in "--- "/"+++ " lines. "a/" and "b/" are the
# defaults; c/ i/ o/ w/ appear when diff.mnemonicPrefix is set (commit/index/
# object/worktree). Strip any of them so a user's mnemonicPrefix config can't
# leave "file" as e.g. "w/foo.py" (which would break existing-file reads and
# point HTML locations at nonexistent paths).
GIT_DIFF_PREFIXES = ("a/", "b/", "c/", "i/", "o/", "w/")


def strip_prefix(path):
    return path[2:] if path.startswith(GIT_DIFF_PREFIXES) else path


def parse(diff_text):
    hunks = []
    new_path = None  # path from the most recent "+++ " line (/dev/null when deleted)
    old_path = None  # path from the most recent "--- " line
    current = None
    counter = 0

    def close():
        if current is not None:
            hunks.append(current)

    for line in diff_text.splitlines():
        if line.startswith("diff --git"):
            # New file boundary. Finalize the previous file's hunk and reset path
            # info; otherwise the next file's non-@@ preamble lines (similarity
            # index, etc.) get appended to the previous file's last hunk.
            close()
            current = None
            new_path = None
            old_path = None
            continue
        m = HUNK_HEADER.match(line)
        if m:
            close()
            counter += 1
            # A deleted file's "+++" is "/dev/null", so use the "--- " path there.
            file_path = old_path if new_path == "/dev/null" else new_path
            current = {
                "id": f"h{counter:03d}",
                "file": file_path,
                "header": line,
                "old_start": int(m.group(1)),
                "new_start": int(m.group(2)),
                "lines": [],
            }
            continue
        if current is None:
            # Only interpret metadata lines while outside a hunk body.
            if line.startswith("+++ "):
                new_path = strip_prefix(line[4:])
            elif line.startswith("--- "):
                old_path = strip_prefix(line[4:])
            # "index " lines and rename/similarity preambles are discarded.
            continue
        if line.startswith("\\"):  # "\ No newline at end of file"
            continue
        # Inside a hunk body, only a leading "+"/"-" marks add/del; nothing else
        # is read as text. This keeps a deleted "-- comment" line (which becomes
        # "--- comment") from being mistaken for a "--- " metadata line.
        if line.startswith("+"):
            current["lines"].append({"type": "add", "content": line[1:]})
        elif line.startswith("-"):
            current["lines"].append({"type": "del", "content": line[1:]})
        else:
            # Context line: strip one leading space (blank lines stay "").
            current["lines"].append({"type": "context", "content": line[1:] if line.startswith(" ") else line})
    close()

    added = sum(1 for h in hunks for l in h["lines"] if l["type"] == "add")
    removed = sum(1 for h in hunks for l in h["lines"] if l["type"] == "del")
    files = len({h["file"] for h in hunks})
    return {
        "stats": {"files": files, "hunks": len(hunks), "added": added, "removed": removed},
        "hunks": hunks,
    }


def annotate(data):
    blocks = []
    for h in data["hunks"]:
        body = []
        for l in h["lines"]:
            prefix = {"add": "+", "del": "-", "context": " "}[l["type"]]
            body.append(prefix + l["content"])
        blocks.append(f"### [{h['id']}] {h['file']}\n{h['header']}\n" + "\n".join(body))
    return "\n\n".join(blocks)


def main():
    if len(sys.argv) != 2:
        print("usage: parse_diff.py <hunks_out_path>", file=sys.stderr)
        sys.exit(2)
    diff_text = sys.stdin.read()
    data = parse(diff_text)
    with open(sys.argv[1], "w") as f:
        json.dump(data, f, ensure_ascii=False, indent=2)
    sys.stdout.write(annotate(data))


if __name__ == "__main__":
    main()
