---
name: create-pr
description: Prepares commits and opens GitHub pull requests, and keeps existing PRs up to date (follow-up pushes, body, title). Use when creating or updating a PR — instructions like "draft PR つくって", "PR Description 更新して", "commit して PR 作成". Read before running gh pr create or gh pr edit.
---

# Creating Pull Requests

Conventions distilled from what gets corrected most often. Nearly every past correction falls into
three buckets: the body is too long, the commit history is messy, or the body no longer matches
what the branch actually does.

**Announce at start:** "Using create-pr skill to open this PR." (or "…to update this PR")

Two paths run through the same rules: opening a new PR (steps 1-6) and pushing an update to a PR
that already exists (step 7). Steps 1, 2, and 5 apply to both.

## Steps

### 1. Verify before opening

Run tests, lint, and build, and confirm they pass before opening the PR. Follow the
verify-completion skill for how to verify. What goes in the body is not the verification output —
it is only what a human still has to check (see step 5).

### 2. Clean up the commits

Commit in meaningful units. Never mix unrelated changes into one PR. Commit subjects follow the
same `type(scope): summary` convention as PR titles (step 4).

Never hand over a branch that still carries `fixup!` commits. Folding a fix in is two moves, not
one: `git commit --fixup <sha>`, then `git rebase --autosquash` to absorb it. Finish both before
pushing, so what the reviewer opens is finished commits. A fix already sitting in a plain commit
gets the same treatment — rewind it (`git reset`) and re-commit as a fixup; the end state is what
matters.

Whether you may rewrite history at all depends on one thing — has anyone reviewed this PR yet?

```bash
gh pr view --json reviews -q '.reviews | length'   # 0 = not yet reviewed
```

| Situation | How to add changes | How to catch up with the base |
| --- | --- | --- |
| Opening a new PR | fixup + autosquash into the commit each change belongs to | rebase onto the latest base |
| Updating a PR nobody has reviewed yet | same as opening, then `git push --force-with-lease` | rebase onto the latest base |
| Updating a PR reviewed at least once | stack new commits on top | merge the base branch in — rebasing counts as rewriting |

The reason for the split: once someone has reviewed, rewriting history destroys the incremental
diff they use to re-review. Before that, nobody loses anything.

Order of operations when history may be rewritten: create the fixup commits first, then one
`git rebase --autosquash origin/<base>` absorbs them and brings the branch up to date in the same
pass. The step 1 verification counts only if it ran against the exact tree you are about to push —
a run from before a rebase, rewrite, or further commit no longer counts.

### 3. Branch

- Match the prefix used by the majority of existing branches in the repo. Where the convention is
  a GitHub account name prefix, that is `matsuyoshi30/`
- For a change spanning multiple repositories, use the identical branch name in every repository
  (the preview environment matches branches by name)
- When one repository needs multiple PRs, stack them, and run preview verification from the
  branch of the last PR in the stack

### 4. Open as a draft

`gh pr create --draft` is the default.

Base branch: use the one that was explicitly named — by the instructions, or by the stack you are
building on, where the base is the parent PR's branch. Otherwise use the repository's default
branch, which is what `gh repo view --json defaultBranchRef -q .defaultBranchRef.name` returns —
do not assume it from the repository's name or from another repository's convention. Either way,
pass `--base` explicitly rather than relying on the default.

Title format: `type(scope): summary`, e.g. `fix(graphql): gRPC 由来の例外を Sentry に送信する`.
Write the summary in whatever language the repo's recent PR titles use. Where the repo puts ticket
IDs in titles, include the ID — and list all of them when the PR covers several tickets.

### 5. Write the body

Follow the headings in the repo's `.github/PULL_REQUEST_TEMPLATE.md` when one exists. When the
template has no verification heading, append one (`## Test plan` or `## 動作確認`, matching the
body language) after the template's sections — do not fold the checklist into What. Without a
template, use Why / What / verification.

Language: match the repo's convention — read the template and a few recent merged PRs and write in
the language they use. When that language is Japanese, write in plain form (常体), not です・ます調.

How to write it:

- Keep it short. Having all the information is not the goal; compress it for the reader
- Lead with Why. What was wrong, and why this change is needed
- What is the design decisions and intent, not a list of changes. Do not restate the diff
- Put the Linear ticket or Sentry issue link in the Why section
- Call out anything still undecided that the reviewer should judge. Do not paper over it
- The verification section lists only what a human has to see with their own eyes, as an unchecked
  checklist. Never list items CI verifies automatically, such as tests or lint passing

Length target: the reader can take in the whole thing without scrolling. Leave out the
investigation trail, the alternatives you tried, and designs you discarded along the way.

### 6. After opening

- Do not post comments on the PR. Do not add self-review comments unprompted
- Link the PR URL on the Linear ticket
- Deploy to the preview environment when the change needs verification there

### 7. Updating an existing PR

Verify (step 1) and clean up the commits (step 2) exactly as you would when opening — the
reviewed-or-not check in step 2 decides whether you squash or stack.

Then bring the body back in line with reality using `gh pr edit`. This is the single most
frequently corrected item — after every push, check whether the body needs updating.

- Rewrite What to cover every commit as of now
- Add anything newly learned to Why
- Check the title still matches the content
- Do not reply to review comments on the PR unless asked to

## Red Flags

Stop and fix when any of these is true.

- The body does not fit on one screen
- The body is in Japanese and uses です・ます調
- The verification section is filled with CI-run items like `./gradlew ...`
- A `fixup!` commit is still in the branch you are about to push
- You rewrote history on a PR that has already been reviewed
- You pushed and never looked at the body
- You are about to post a comment on the PR
- The body describes the investigation trail or rejected alternatives

## Common Mistakes

| Mistake | Fix |
| --- | --- |
| Listing changed files and methods | Explain why the design is what it is. The diff speaks for itself |
| Dumping everything you verified into Test plan | Drop what CI checks. Keep only what needs human judgment |
| Leaving your open concerns out of the body | State them as points for the reviewer to confirm |
| Stacking a new commit for every fix on an unreviewed PR | Fold it into the relevant commit and autosquash |
| Leaving `fixup!` commits in the pushed branch | `git rebase --autosquash` before pushing. Fixup is half the move |
| Opening as ready for review | Open as a draft. Mark ready only when asked |
