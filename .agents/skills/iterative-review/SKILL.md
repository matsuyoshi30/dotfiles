---
name: iterative-review
description: Run an independent code review, fix Blocker and Follow-up findings, and repeat to the iteration limit. Use when an automated review-fix cycle is requested.
allowed-tools: Agent(review-agent, fix-agent), Bash, Read, Glob, Grep
---

# Iterative Review

Orchestrate an automated improvement loop: a **review subagent** analyzes code, then a **fix subagent** addresses the findings, repeating until quality gates are met.

## Parameters

- **Target** (optional argument): file paths or directories to narrow the review to. If omitted, review the whole local change.
- **Max iterations**: 3 (hardcoded).

## Step 1 — Determine Review Target

The target is the local change: the diff from the merge-base with `origin/HEAD` to the working tree, which covers the branch's commits, staged and unstaged edits, and untracked files. If the user provided file paths or directories, they narrow it; a named file with no change is reviewed whole.

Create one working directory for the run (`mktemp -d`, under the session scratchpad when there is one). Each iteration prepares the target afresh in its own subdirectory, because the fixes change the diff.

## Step 2 — Run the Loop

For each iteration (max 3):

### 2a. Spawn Review Subagent

Prepare the target into `<run dir>/iter-<n>`:

```bash
~/.claude/skills/reviewing-code/scripts/prepare_target.sh <run dir>/iter-<n> local [path...]
```

If the script fails, report its error and stop. If `diff.txt` is empty, there is nothing to review: skip to Step 3.

Read [review-prompt.md](review-prompt.md) and fill in the placeholders:
- `{cwd}` — current working directory
- `{target_dir}` — the directory just prepared

Launch an Agent with `subagent_type: "review-agent"` using the filled prompt. Do NOT use any other subagent type.

Wait for the review result.

### 2b. Check Exit Condition

Parse the `---SUMMARY---` block from the review output.

- If **Blocker = 0 AND Follow-up = 0**: nothing fixable remains. Skip to Step 3.
- Otherwise: continue to 2c. Needs decision, Question, and Nits never keep the loop going: the first two need a human, and Nits need no fix.
- If counts cannot be parsed: check whether the review text contains any `#### [Blocker]` or `#### [Follow-up]` headings. If none, treat as resolved and skip to Step 3. Otherwise, treat as "issues remain" and continue.

### 2c. Spawn Fix Subagent

Read [fix-prompt.md](fix-prompt.md) and fill in the placeholders:
- `{cwd}` — current working directory
- `{review_output}` — full review output from 2a

Launch an Agent with `subagent_type: "fix-agent"` using the filled prompt.

Wait for the fix result.

### 2d. Next Iteration

If the max iteration count has not been reached, go back to 2a to re-review the now-modified code.

## Step 3 — Final Report

After the loop ends, present a consolidated report to the user:

```markdown
## Iterative Review Complete

**Iterations**: {iterations_run} / 3
**Exit reason**: {No fixable findings remain | Max iterations reached}

### Iteration 1
**Review**: {blocker} Blocker, {needs_decision} Needs decision, {question} Question, {follow_up} Follow-up, {nits} Nits
**Fixed**: {summary of what was fixed}

### Iteration 2 (if applicable)
**Review**: {blocker} Blocker, {needs_decision} Needs decision, {question} Question, {follow_up} Follow-up, {nits} Nits
**Fixed**: {summary of what was fixed}

### Iteration 3 (if applicable)
**Review**: {blocker} Blocker, {needs_decision} Needs decision, {question} Question, {follow_up} Follow-up, {nits} Nits
**Fixed**: {summary of what was fixed}

### Needs a Human
{List every Needs decision and Question from the final review with its decision or question, or "None."}

### Remaining Issues
{List Nits from the final review. On a max-iterations exit, also list the final review's Blockers and Follow-ups that the last fix report did not mark as fixed, labelled "not re-reviewed". Or "None."}
```

## Important Rules

- **Use ONLY `review-agent` and `fix-agent` subagent types.**. Do not use any other subagent type. The `review-agent` definition already points at the `reviewing-code` skill.
- **Do not modify code yourself.** All code changes happen through the fix subagent.
- **Do not skip the review subagent.** Even if you think you know the issues, always run the reviewer.
- **Preserve the structured summary format.** The `---SUMMARY---` block is required for the exit condition check.
- **Run subagents sequentially.** Each step depends on the previous result — never parallelize review and fix within the same iteration.
- **Across iterations, review and fix subagents are independent.** Each gets a fresh prompt with full context — do not rely on prior subagent state.
