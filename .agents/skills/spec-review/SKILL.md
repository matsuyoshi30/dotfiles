---
name: spec-review
description: Spawns subagents to review implementation against spec/requirements, fix gaps, and re-review until all spec compliance issues are resolved or the iteration limit is hit. Use after implementing features to verify the code matches what was requested — nothing more, nothing less.
allowed-tools: Agent(spec-review-agent, fix-agent), Bash, Read, Glob, Grep
user-invocable: true
---

# Spec Compliance Review-Fix Loop

Orchestrate an automated spec compliance loop: a **spec-review-agent** verifies implementation against requirements, then a **fix-agent** addresses gaps, repeating until the implementation fully matches the spec.

This skill complements `iterative-review` (code quality). Use this skill first — building the wrong thing well is still wrong.

## Parameters

- **Spec** (required argument): The specification to review against. Accepts:
  - GitHub issue URL (`gh issue view` で取得)
  - File path to a design document or spec
  - Inline text describing requirements
- **Target** (optional): File paths or directories to review. If omitted, falls back to git diff (staged + unstaged). When a directory is given, collect source files only — exclude `node_modules`, `dist`, `build`, `.git`, `vendor`, `__pycache__`, binary files.
- **Max iterations**: 2 (hardcoded).

## Step 0 — Resolve Spec

Parse the spec argument:

- **GitHub issue URL**: Extract requirements via `gh issue view <url> --json title,body`
- **File path**: Read the file content
- **Inline text**: Use as-is

The resolved spec text is passed verbatim to review subagents. Do not summarize or interpret it.

## Step 1 — Determine Review Target

Same logic as `iterative-review`:

### 1a. User-specified target (highest priority)

If file paths or directories were provided, use those.

### 1b. PR diff + local unpushed changes

```bash
gh pr view --json baseRefName 2>/dev/null
```

If a PR exists, collect files from both PR diff and local changes, deduplicate.

### 1c. Local git diff (fallback)

```bash
git diff --name-only
git diff --name-only --cached
```

If no files found, inform the user and stop.

## Step 2 — Run the Loop

For each iteration (max 2):

### 2a. Spawn Spec Review Subagent

Read [review-prompt.md](review-prompt.md) and fill in the placeholders:
- `{cwd}` — current working directory
- `{resolved_spec_text}` — the resolved specification text
- `{target_files}` — list of files to review

Launch an Agent with `subagent_type: "spec-review-agent"` using the filled prompt. Do NOT use `review-agent` or any other subagent type.

Wait for the review result.

### 2b. Check Exit Condition

Parse the `---SUMMARY---` block from the review output.

- If **all counts are 0** (MISSING, EXTRA, MISUNDERSTOOD): spec compliance confirmed. Skip to Step 3.
- If counts cannot be parsed: check whether the review text contains actual findings. If none, treat as resolved. Otherwise, treat as "issues remain" and continue.
- Otherwise: continue to fix.

### 2c. Spawn Fix Subagent

Read [fix-prompt.md](fix-prompt.md) and fill in the placeholders:
- `{cwd}` — current working directory
- `{resolved_spec_text}` — the specification
- `{review_output}` — full review output from 2a

Launch an Agent with `subagent_type: "fix-agent"` using the filled prompt.

Wait for the fix result.

### 2d. Next Iteration

If the max iteration count has not been reached, go back to 2a to re-review the now-modified code.

## Step 3 — Final Report

```markdown
## Spec Compliance Review Complete

**Iterations**: {iterations_run} / 2
**Exit reason**: {All spec requirements met | Max iterations reached}

### Specification
{spec source — issue URL, file path, or "(inline)"}

### Iteration 1
**Review**: {missing} Missing, {extra} Extra, {misunderstood} Misunderstood
**Fixed**: {summary}

### Iteration 2 (if applicable)
**Review**: {missing} Missing, {extra} Extra, {misunderstood} Misunderstood
**Fixed**: {summary}

### Remaining Issues
{List any unresolved findings, or "None — implementation matches spec."}
```

## Important Rules

- **Use ONLY `spec-review-agent` and `fix-agent` subagent types.** Do not use any other subagent type.
- **Do not modify code yourself.** All code changes happen through the fix subagent.
- **Do not skip the review subagent.** Always run the reviewer even if you think compliance is obvious.
- **Preserve the structured summary format.** The `---SUMMARY---` block is required for the exit condition check.
- **Run subagents sequentially.** Each step depends on the previous result — never parallelize review and fix within the same iteration.
- **Across iterations, review and fix subagents are independent.** Each gets a fresh prompt with full context — do not rely on prior subagent state.
- **This skill checks spec compliance, not code quality.** Use `iterative-review` for quality concerns.
