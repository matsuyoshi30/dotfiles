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


def unquote_c(path):
    """Undo git's C-style quoting (core.quotePath): "\\346\\227\\245..." etc.

    git octal-escapes each raw byte, so decode escapes to latin-1 bytes first,
    then reinterpret as UTF-8. A path that fails to decode is returned unquoted
    but otherwise as-is (better a odd name than a crash).
    """
    if len(path) >= 2 and path[0] == '"' and path[-1] == '"':
        body = path[1:-1]
        try:
            return body.encode("utf-8").decode("unicode_escape").encode("latin-1").decode("utf-8")
        except UnicodeError:
            return body
    return path


def strip_prefix(path):
    # Order matters: git appends a disambiguating tab after names containing
    # spaces, and C-quotes names with non-ASCII bytes; the quoted form contains
    # the a// b/ prefix inside the quotes.
    if path.endswith("\t"):
        path = path[:-1]
    path = unquote_c(path)
    return path[2:] if path.startswith(GIT_DIFF_PREFIXES) else path


def header_new_path(line):
    """Best-effort new-side path from a `diff --git a/X b/Y` header.

    Needed for sections that never emit "--- "/"+++ " lines (pure renames,
    mode-only changes, binary files). Split at the rightmost " b/" (or its
    quoted form) — ambiguous only if the old name itself contains " b/".
    """
    s = line[len("diff --git "):]
    for marker in (' "b/', " b/"):
        i = s.rfind(marker)
        if i != -1:
            return strip_prefix(s[i + 1:])
    return None


def parse(diff_text):
    hunks = []
    new_path = None  # path from the most recent "+++ " line (/dev/null when deleted)
    old_path = None  # path from the most recent "--- " line
    current = None
    counter = 0
    files_total = 0       # every `diff --git` section, hunk-bearing or not
    no_hunk_files = []    # sections with 0 hunks: pure renames, mode-only, binary
    section_path = None
    section_had_hunk = False
    combined = False
    new_no = 0  # only meaningful inside a hunk body; reset by each "@@" header
    old_no = 0

    def close():
        if current is not None:
            hunks.append(current)

    def close_section():
        nonlocal section_path, section_had_hunk
        if section_path is not None and not section_had_hunk:
            no_hunk_files.append(section_path)
        section_path = None
        section_had_hunk = False

    # Split on "\n" only. git separates diff lines with plain \n; splitlines()
    # would also split on \f, \v, \x85, U+2028/U+2029 and lone \r appearing
    # INSIDE a source line, letting file content forge diff structure (fake
    # deletions, forged hunk headers, swallowed lines).
    lines = diff_text.split("\n")
    if lines and lines[-1] == "":
        lines.pop()
    for line in lines:
        if line.startswith("diff --git"):
            # New file boundary. Finalize the previous file's hunk and reset path
            # info; otherwise the next file's non-@@ preamble lines (similarity
            # index, etc.) get appended to the previous file's last hunk.
            close()
            close_section()
            current = None
            new_path = None
            old_path = None
            files_total += 1
            section_path = header_new_path(line)
            continue
        if line.startswith("@@@"):
            # Combined diff (-c/--cc) is unsupported; flag it loudly instead of
            # silently dropping its hunks.
            combined = True
            continue
        m = HUNK_HEADER.match(line)
        if m:
            close()
            counter += 1
            section_had_hunk = True
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
            # Two independent counters: the new side advances on add+context, the
            # old side on del+context. A single counter drifts as soon as adds and
            # dels interleave, and every reported line number after that is wrong.
            new_no = current["new_start"]
            old_no = current["old_start"]
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
        # "no" is the line number shown to the reviewer: the new-side number for
        # add/context (what the file on disk has now) and the old-side number for
        # del (which has no new-side number at all).
        if line.startswith("+"):
            current["lines"].append({"type": "add", "no": new_no, "content": line[1:]})
            new_no += 1
        elif line.startswith("-"):
            current["lines"].append({"type": "del", "no": old_no, "content": line[1:]})
            old_no += 1
        else:
            # Context line: strip one leading space (blank lines stay "").
            current["lines"].append(
                {"type": "context", "no": new_no,
                 "content": line[1:] if line.startswith(" ") else line})
            new_no += 1
            old_no += 1
    close()
    close_section()

    added = sum(1 for h in hunks for l in h["lines"] if l["type"] == "add")
    removed = sum(1 for h in hunks for l in h["lines"] if l["type"] == "del")
    files = len({h["file"] for h in hunks})
    return {
        "stats": {
            "files": files,
            "files_total": files_total,
            "no_hunk_files": no_hunk_files,
            "hunks": len(hunks),
            "added": added,
            "removed": removed,
        },
        "hunks": hunks,
    }, combined


# Marker printed in the annotated diff. Context gets a spelled-out "ctx" rather
# than the unified format's bare leading space: with -U15 a hunk is mostly
# context, and a one-space difference is too weak a signal — reviewers read
# unchanged code as part of the change and report it as introduced by the diff.
# "+"/"-" stay conventional (models pattern-match those reliably).
MARKER = {"add": "+", "del": "-", "context": "ctx"}


def compress(numbers):
    """[3,4,5,9] -> "3-5,9". Used for the per-hunk changed-line summary."""
    if not numbers:
        return "none"
    parts = []
    start = prev = numbers[0]
    for n in numbers[1:]:
        if n == prev + 1:
            prev = n
            continue
        parts.append(str(start) if start == prev else f"{start}-{prev}")
        start = prev = n
    parts.append(str(start) if start == prev else f"{start}-{prev}")
    return ",".join(parts)


def annotate(data):
    blocks = []
    for h in data["hunks"]:
        body = []
        for l in h["lines"]:
            # Anchor form "+124" / "-45" / "ctx120" — the reviewer copies this
            # verbatim into a finding's `anchor`, so it must match render.py's
            # parser exactly.
            body.append(f"{MARKER[l['type']] + str(l['no']):<8}| {l['content']}")
        added = compress([l["no"] for l in h["lines"] if l["type"] == "add"])
        deleted = compress([l["no"] for l in h["lines"] if l["type"] == "del"])
        # Ground truth the reviewer can self-check against without re-scanning
        # ~30 context lines per hunk.
        head = (f"### [{h['id']}] {h['file']} | "
                f"added(new): {added} | deleted(old): {deleted}")
        blocks.append(f"{head}\n{h['header']}\n" + "\n".join(body))
    return "\n\n".join(blocks)


def main():
    if len(sys.argv) != 2:
        print("usage: parse_diff.py <hunks_out_path>", file=sys.stderr)
        sys.exit(2)
    if hasattr(sys.stdin, "reconfigure"):
        sys.stdin.reconfigure(encoding="utf-8", errors="replace")
        sys.stdout.reconfigure(encoding="utf-8")
    diff_text = sys.stdin.read()
    data, combined = parse(diff_text)
    with open(sys.argv[1], "w", encoding="utf-8") as f:
        json.dump(data, f, ensure_ascii=False, indent=2)
    sys.stdout.write(annotate(data))
    if combined:
        print("warning: combined diff (@@@ hunks, from -c/--cc) is unsupported; "
              "its hunks were dropped", file=sys.stderr)
        sys.exit(2)


if __name__ == "__main__":
    main()
