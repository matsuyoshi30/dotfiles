---
name: iterative-review
description: Spawns subagents to review code, fix issues, and re-review until Critical/High findings reach zero or the iteration limit is hit. Triggers when iterative code quality improvement is needed, such as after implementing features, before merging branches, or when the user requests an automated review-fix cycle.
allowed-tools: Agent, Bash, Read, Glob, Grep
user-invocable: true
---

# Review-Fix Loop

Orchestrate an automated improvement loop: a **review subagent** analyzes code, then a **fix subagent** addresses the findings, repeating until quality gates are met.

## Parameters

- **Target** (optional argument): file paths or directories to review. If omitted, falls back to git diff (staged + unstaged changes). When a directory is given, collect source files only — exclude common non-source paths (`node_modules`, `dist`, `build`, `.git`, `vendor`, `__pycache__`, binary files).
- **Max iterations**: 3 (hardcoded).

## Step 1 — Determine Review Target

Resolve the target files using the following priority:

### 1a. User-specified target (highest priority)

If the user provided file paths or directories as arguments, use those.

### 1b. PR diff + local unpushed changes (highest auto-detect priority)

If no target was specified, check whether the current branch has an open pull request:

```bash
gh pr view --json baseRefName 2>/dev/null
```

If a PR exists, collect files from **both** sources and deduplicate:

1. PR diff (changes against base branch):
   ```bash
   gh pr diff --name-only
   ```
2. Local unpushed changes (staged + unstaged):
   ```bash
   git diff --name-only
   git diff --name-only --cached
   ```

### 1c. Local git diff (fallback)

If no PR exists, fall back to local changes only:

```bash
git diff --name-only
git diff --name-only --cached
```

Collect the union of both outputs (deduplicate).

---

If no files are found from any of the above, inform the user and stop.

## Step 2 — Run the Loop

For each iteration (max 3):

### 2a. Spawn Review Subagent

Launch a general-purpose Agent with the following prompt structure:

```
You are a code review agent.

Working directory: {cwd}

First, invoke the code-reviewer skill by calling the Skill tool with skill: "code-reviewer".
Then, apply the loaded review methodology to the following files: {target_files}

IMPORTANT: After the standard review output, append a machine-readable summary block in exactly this format:

---SUMMARY---
CRITICAL: {count}
HIGH: {count}
MEDIUM: {count}
LOW: {count}
---END---

For each Critical, High, and Medium issue, include the file path, line number, description, and a specific recommended fix.
```

Wait for the review result.

### 2b. Check Exit Condition

Parse the `---SUMMARY---` block from the review output.

- If **all counts are 0** (Critical, High, Medium, and Low): no issues remain. Skip to Step 3.
- If **Critical = 0 AND High = 0** but Medium or Low remain: continue the loop (fix Medium issues) until the max iteration limit is reached.
- If counts cannot be parsed: check whether the review text contains any severity section headers ("## Critical Issues", "## High Priority Issues", "## Medium Priority Issues") with actual findings listed beneath them. If none have content, treat as resolved and skip to Step 3. Otherwise, treat as "issues remain" and continue.

### 2c. Spawn Fix Subagent

Launch an Agent with `mode: "auto"` and the following prompt structure:

```
You are a code fix agent. Apply the following review findings to the codebase.

Working directory: {cwd}

## Review Findings

{paste the full review output from 2a}

## Instructions

1. Fix issues in priority order: Critical → High → Medium. Leave Low issues unfixed (they are reported only).
2. For each fix:
   - Read the relevant file to understand context before editing.
   - Make the minimal change that addresses the finding.
   - Do NOT refactor unrelated code or add unrelated improvements.
3. After all fixes are applied, run verification until everything passes:
   a. **Lint**: Check CLAUDE.md, package.json scripts, Makefile, or config files (e.g., .eslintrc, pyproject.toml) for lint commands. Run if found.
   b. **Tests**: Check CLAUDE.md or README.md for documented test commands. Otherwise check for package.json "test" script, Makefile test target, or language-standard runners (go test ./..., pytest, cargo test). Prefer unit tests over integration/e2e tests if distinguishable.
   c. **Fix failures**: If lint or tests fail due to your changes, fix the issues and re-run until both pass. Do not proceed with failures outstanding.
   d. If no lint or test runner is found, skip this step.
4. Summarize what you changed, what lint/test commands you ran, and their final results.
```

Wait for the fix result.

### 2d. Next Iteration

If the max iteration count has not been reached, go back to 2a to re-review the now-modified code.

## Step 3 — Final Report

After the loop ends, present a consolidated report to the user:

```markdown
## Review-Fix Loop Complete

**Iterations**: {iterations_run} / 3
**Exit reason**: {Critical/High resolved | Max iterations reached}

### Iteration 1
**Review**: {critical} Critical, {high} High, {medium} Medium, {low} Low
**Fixed**: {summary of what was fixed}

### Iteration 2 (if applicable)
**Review**: {critical} Critical, {high} High, {medium} Medium, {low} Low
**Fixed**: {summary of what was fixed}

### Iteration 3 (if applicable)
**Review**: {critical} Critical, {high} High, {medium} Medium, {low} Low
**Fixed**: {summary of what was fixed}

### Remaining Issues
{List any Medium/Low findings from the final review, or "None — all issues resolved."}
```

## Important Rules

- **Do not modify code yourself.** All code changes happen through the fix subagent.
- **Do not skip the review subagent.** Even if you think you know the issues, always run the reviewer.
- **Preserve the structured summary format.** The `---SUMMARY---` block is required for the exit condition check.
- **Run subagents sequentially.** Each step depends on the previous result — never parallelize review and fix within the same iteration.
- **Across iterations, review and fix subagents are independent.** Each gets a fresh prompt with full context — do not rely on prior subagent state.
