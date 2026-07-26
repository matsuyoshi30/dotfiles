---
name: diff-review
description: Generates a local static HTML review screen for a largish diff - groups the changes by intent, orders them by risk with explanations, produces findings in two stages (a blind review with the plan hidden, then a plan cross-check), and collects the human's adopt/reject decisions and comments into a feedback markdown. Use when a diff is too large to review comfortably in the terminal, or when the user asks for a diff review / review screen. Invoke explicitly with `/diff-review [plan-path] [target-paths...]`.
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
The scripts need only the Python 3 standard library; nothing to install.

Copy this checklist into your visible reply and check off items as you
complete them (no separate file needed):

```
Diff review progress:
- [ ] Step 1: Decide the target diff
- [ ] Step 2: Resolve the plan
- [ ] Step 3: Blind review (plan withheld)
- [ ] Step 4: Plan cross-check (skip if no plan)
- [ ] Step 5: Render the HTML and open it
- [ ] Steps 6-7: Hand off to the human (no auto-fix)
```

## Step 1: Decide the target diff

First fix the target repository: `REPO=$(git -C <invocation cwd> rev-parse
--show-toplevel)`. Every later step that says "the target repository" means
this `$REPO` — the argument existence check, the plan search, and the diff
capture all anchor here.

Argument classification (when arguments are passed): an argument that is an
existing `.md` file — existence checked relative to `$REPO`, not the shell's
current directory — is the plan path (consumed by Step 2); every other argument
is a diff target pathspec. An argument ending in `.md` that does NOT exist is
almost certainly a mistyped plan path: stop and tell the user instead of
silently reclassifying it as a pathspec (which would yield an empty diff and an
unrelated "diff is empty" message). Echo the classification result in one line
("plan: X / diff targets: Y") before proceeding.

Same precedence as `iterative-review`:
1. If diff target pathspecs were passed as arguments, use them as the target of `git diff --no-ext-diff HEAD -- <paths>`.
2. Otherwise check for a PR with `gh pr view --json baseRefName`; if one exists, use `gh pr diff`.
3. If there is no PR, use the local diff (working tree vs HEAD).

Take a generous amount of context. Local: `git --no-pager diff --no-ext-diff HEAD -U15`.
PR: `gh pr diff`. If the diff is empty, tell the user and stop. This step only
decides which command to use — the single actual capture happens in Step 3's
command block.

Also check `git -C "$REPO" status --porcelain` for untracked files (`??`):
`git diff HEAD` does not include them, so a new file that was never `git add`ed
is invisible to this whole flow. List any untracked files to the user as
outside the review (or suggest `git add -N` to include them).

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

## Step 2: Resolve the plan (path only — do not read it yet)

- If a plan path is passed as an argument, use it.
- Otherwise search `plans/*.md`, `.matsuyoshi30/plans/*.md`, and
  `.matsuyoshi/plans/*.md` — non-recursive, rooted at the target repository
  (the repo the diff came from), not the shell's current directory. If several match, pick the most recently modified one and
  tell the user which was used.
- If none is found, proceed with "no plan cross-check" (Step 4 then consists
  only of the mandatory `plan_checked: false` / `plan_path: null` write).

Resolve only the path in this step; do not open the plan until Step 4. You are
the context that assembles the blind reviewer's prompt in Step 3 — a plan you
have already read would leak into it. Keep the path for Step 4, which writes it
into review.json as `plan_path` (repo-relative).

## Step 3: Blind review (do not pass the plan)

First convert the diff into hunks.json and an annotated diff:

```bash
# Write the diff chosen in Step 1 to /tmp/dr-diff.txt (the same diff, not a
# second acquisition). $REPO comes from Step 1 — anchor with -C so a shell
# sitting in some other repo can't silently capture the wrong (non-empty!)
# diff. The :(exclude) pathspecs keep plan/worklog files OUT of the diff:
# without them, a committed or unstaged plan rides into the blind reviewer's
# prompt as diff content and the blind stage silently stops being blind.
git -C "$REPO" --no-pager diff --no-ext-diff HEAD -U15 -- . \
  ':(exclude)plans/' ':(exclude).matsuyoshi*/' ':(exclude).devflow/' \
  ':(exclude)docs/superpowers/plans/' ':(exclude)*PLAN.md' ':(exclude)*WORKLOG.md' \
  > /tmp/dr-diff.txt
: > /tmp/dr-review.json; : > /tmp/dr-hunks.json; : > /tmp/dr-annotated.txt   # truncate stale files from a previous run (truncation, not rm: /tmp deletes can be permission-blocked; an empty file fails loudly downstream instead of silently reusing the old repo's data)
[ -s /tmp/dr-diff.txt ] || { echo "diff is empty"; exit 1; }     # if this exits, tell the user and stop
python3 {skill_dir}/scripts/parse_diff.py /tmp/dr-hunks.json < /tmp/dr-diff.txt > /tmp/dr-annotated.txt
```

If the exclusions removed any changed paths (compare `git -C "$REPO" diff
--no-ext-diff HEAD --name-only` with the captured diff), tell the user which
paths were excluded from the review. For the PR path (`gh -C "$REPO" pr diff`,
which cannot take pathspecs): after capture, grep `^diff --git` in
/tmp/dr-diff.txt for those same plan/worklog paths; if any appear, re-capture
locally against the PR base with the excludes
(`git -C "$REPO" diff --no-ext-diff -U15 origin/<baseRefName>...HEAD -- . <excludes>`).

Note: parse_diff.py only counts files that have hunks. Pure renames, mode changes,
and binary diffs have 0 hunks, so they do not appear in stats.files/hunks; for a
diff dominated by such changes the stats undercount the real number of changed files.
If stats.hunks is 0 while the diff is non-empty (a diff of only such changes),
there is nothing to review at hunk level: tell the user what the diff consists
of and stop.

Launch the blind reviewer. This is the only tool-specific part:

- Claude Code: Agent tool, `subagent_type: "diff-blind-reviewer"`. Read
  `{skill_dir}/blind-review-prompt.md` and fill in `{cwd}` and `{annotated_diff}`
  (the contents of /tmp/dr-annotated.txt) before passing it.
- Fallback (any harness) — use when the Agent tool is unavailable OR its reply
  is lost (no result after completion, notification never arrives): launch a
  fresh read-only subprocess with the same prompt, cwd = the target repo.
  Claude Code: write the filled prompt to /tmp/dr-blind-prompt.txt (never
  inside the repo — it would pollute the next diff) and run
  `claude -p --allowedTools Read Glob Grep < /tmp/dr-blind-prompt.txt` (pass
  the prompt on stdin — appended after `--allowedTools` it is swallowed as a
  flag value). Codex: `codex exec --sandbox read-only`. Note: a subprocess can physically read the
  plan, so the ban rests on the prompt's forbidden-paths instruction alone (the
  same paths as the diff-blind-reviewer.md agent definition) — acceptable, but
  say in your summary which launch path was used.

The blind reviewer only returns text — it cannot write files. You extract the
```json fenced block from its reply and write it to `/tmp/dr-review.json`
yourself (render.py also accepts the file with the fence markers left in; it
strips them on load). If the reviewer runs asynchronously, wait for its
completion and use the reply it returns; do not scrape its transcript files.
If no reply has arrived after a couple of minutes past the reviewer's expected
runtime (~2-5 min for a mid-size diff), treat it as lost: re-launch via the
fallback path above instead of waiting indefinitely, and ignore the original
reviewer's result if it surfaces later (don't merge two reviews). The blind schema has no plan fields — `plan_checked` /
`plan_path` are added to review.json in Step 4.

## Step 4: Plan cross-check (only when a plan exists)

Follow `{skill_dir}/plan-crosscheck.md`: cross-check each finding in
`/tmp/dr-review.json` against the plan, add a `plan_note`, and add/remove findings
as needed. Finally write `plan_checked: true` and `plan_path` into review.json.

If there is no plan, skip the cross-check itself, but the write is still
mandatory: set `plan_checked: false` / `plan_path: null` as real keys in
review.json.

## Step 5: Render the HTML and open it

```bash
OUT="${TMPDIR:-/tmp}/diff-review-$(date +%Y%m%d-%H%M%S).html"
python3 {skill_dir}/scripts/render.py \
  --hunks /tmp/dr-hunks.json --review /tmp/dr-review.json \
  --template {skill_dir}/scripts/template.html --out "$OUT" \
  2> /tmp/dr-render-stderr.txt
cat /tmp/dr-render-stderr.txt   # must be empty — see validation loop below
[ -s /tmp/dr-render-stderr.txt ] || open "$OUT"   # macOS; never open on warnings/errors — a failed render leaves a truncated or misleading page
```

Validation loop: render.py prints a `warning:` line on stderr when a hunk is in
no group or a group references an unknown hunk id (captured in
/tmp/dr-render-stderr.txt above so "no warnings" is verifiable, not eyeballed).
If either warning appears, fix
`/tmp/dr-review.json` (assign the missing hunks to a group / correct the ids) and
re-run render.py. Only open the HTML once render.py runs warning-free — a warning
means the human would finish the review without seeing part of the diff.
A Python traceback in the stderr file is a different failure: render.py itself
died (non-zero exit), so suspect the structure of review.json rather than hunk
assignments, and do not open the partially written HTML.

When you present the result, tell the user the generated file path (`$OUT` —
note it lands in `$TMPDIR`, not necessarily `/tmp`), and also mention changes
that produce no hunks (pure renames, mode changes, binary files — visible in
`git status` or `gh pr diff --name-only` but absent from the screen, and
undercounted by the screen's stats.files number), so the human does not mistake
the screen for the whole change.

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
