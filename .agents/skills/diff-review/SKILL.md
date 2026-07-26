---
name: diff-review
description: For a largish diff, generate a local static HTML review screen that groups the changes by intent and orders them by risk with explanations, produce findings in two stages (a blind review with the plan hidden, then a plan cross-check), and collect the human's adopt/reject decisions and comments into a feedback markdown. Invoke explicitly with `/diff-review [plan-path]` when the diff is large.
allowed-tools: Agent(diff-blind-reviewer), Bash, Read, Glob, Grep, Write, Edit
user-invocable: true
---

# diff-review

Turn a diff into an explained, human-friendly HTML review, and collect the
adopt/reject decisions and comments into feedback for the next work session. To
resist deference bias, the blind review (an independent context that never sees
the plan) is kept separate from the plan cross-check. This skill does not auto-fix.

The scripts live in `scripts/` (relative to `SKILL.md`). This document writes the
real path as `{skill_dir}`; resolve it from the location of SKILL.md at run time
(`{skill_dir}` is the directory containing this SKILL.md, i.e. the parent of `scripts/`).

## Step 1: Decide the target diff

Same precedence as `iterative-review`:
1. If a target path is passed as an argument, use it as the target of `git diff --no-ext-diff HEAD -- <paths>`.
2. Otherwise check for a PR with `gh pr view --json baseRefName`; if one exists, use `gh pr diff`.
3. If there is no PR, use the local diff (working tree vs HEAD).

Take a generous amount of context. Local: `git --no-pager diff --no-ext-diff HEAD -U15`.
PR: `gh pr diff`. If the diff is empty, tell the user and stop.

Basing on `HEAD` captures both staged and unstaged changes in one consistent diff.
Taking `git diff` and `git diff --cached` separately and concatenating them is
avoided: a file changed in both staged and unstaged becomes two `diff --git`
blocks and its hunks duplicate.

Important: always pass `--no-ext-diff` to `git diff`. If `diff.external`
(difftastic, etc.) is configured, a plain `git diff` does not emit unified format
and parse_diff.py sees 0 hunks. `gh pr diff` returns GitHub's standard unified
diff, so it is unaffected. (Path prefixes are handled by parse_diff.py: it strips
git's `a/ b/ c/ i/ o/ w/` prefixes, so a user's `diff.mnemonicPrefix` setting does
not corrupt file paths.)

## Step 2: Resolve the plan

- If a plan path is passed as an argument, use it.
- Otherwise search `plans/*.md` and `.matsuyoshi30/plans/*.md`. If several match,
  pick the most recently modified one and tell the user which was used.
- If none is found, proceed with "no plan cross-check" (skip Step 4).

## Step 3: Blind review (do not pass the plan)

First convert the diff into hunks.json and an annotated diff:

```bash
# Use the diff source chosen in Step 1. For a local diff:
git --no-pager diff --no-ext-diff HEAD -U15 > /tmp/dr-diff.txt   # for a PR: gh pr diff > /tmp/dr-diff.txt
[ -s /tmp/dr-diff.txt ] || { echo "diff is empty; stopping"; }   # if empty, tell the user and stop
python3 {skill_dir}/scripts/parse_diff.py /tmp/dr-hunks.json < /tmp/dr-diff.txt > /tmp/dr-annotated.txt
```

Note: parse_diff.py only counts files that have hunks. Pure renames, mode changes,
and binary diffs have 0 hunks, so they do not appear in stats.files/hunks; for a
diff dominated by such changes the stats undercount the real number of changed files.

Launch the blind reviewer. This is the only tool-specific part:

- Claude Code: Agent tool, `subagent_type: "diff-blind-reviewer"`. Read
  `{skill_dir}/blind-review-prompt.md` and fill in `{cwd}` and `{annotated_diff}`
  (the contents of /tmp/dr-annotated.txt) before passing it.
- Codex: launch a fresh subprocess with `codex exec --sandbox read-only` whose cwd
  is a non-empty working directory (the real repo), and pass the same prompt.
  Note: a read-only sandbox can still read the plan, so rely on the
  instruction-based ban on referencing the plan in the prompt (the same forbidden
  paths as the diff-blind-reviewer.md agent definition).

Save the blind reviewer's output — the contents of the ```json fenced block only —
to `/tmp/dr-review.json` (saving it with the fence markers makes render.py in Step 5
fail with a JSONDecodeError in `json.load`).

## Step 4: Plan cross-check (only when a plan exists)

Follow `{skill_dir}/plan-crosscheck.md`: cross-check each finding in
`/tmp/dr-review.json` against the plan, add a `plan_note`, and add/remove findings
as needed. Finally write `plan_checked: true` and `plan_path` into review.json.

If there is no plan, skip this and set `plan_checked: false` / `plan_path: null` in
review.json.

## Step 5: Render the HTML and open it

```bash
OUT="${TMPDIR:-/tmp}/diff-review-$(date +%Y%m%d-%H%M%S).html"
python3 {skill_dir}/scripts/render.py \
  --hunks /tmp/dr-hunks.json --review /tmp/dr-review.json \
  --template {skill_dir}/scripts/template.html --out "$OUT"
open "$OUT"   # macOS
```

## Steps 6-7: Human judgement and feedback

In the screen, enter adopt/reject and comments, click "Generate feedback" ->
"Copy to clipboard". Paste the copied markdown back into the original
implementation session to request fixes. The skill ends here. It has no auto-fix
loop (that is the domain of iterative-review / spec-review).

## Important Rules

- Do not pass the plan to the blind review. Do not put the plan's contents, the PR
  description, or the implementation-time discussion into the blind reviewer's context.
- The blind reviewer may read the real repository (to catch misuse of existing
  functions). The only thing forbidden is referencing plan files.
- The generated HTML is fully local. Do not send code contents anywhere external.
- Do not fix the code yourself. This skill goes up to producing the screen and the
  feedback.
