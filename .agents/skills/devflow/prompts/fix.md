<!-- Orchestrator-only: dispatch metadata (not part of the agent's instructions)
- subagent_type: fix-agent
- model: sonnet
- placeholders: {cwd}, {review_output}, {baseline_json_path}
-->

---

Apply the following review findings to the codebase.

Working directory: {cwd}

## Pre-existing Failures (Out of Scope)

Read `{baseline_json_path}` before you start. Signature = `{file, first_error_line}` (normalized). Failures matching a baseline signature are pre-existing and out of scope — do not try to fix them, and do not treat them as blockers in the verification step below.

## Review Findings

{review_output}

## Instructions

1. Fix issues in priority order: Critical → High → Medium (or Missing → Misunderstood → Extra for spec compliance). Leave Low issues unfixed (they are reported only).
2. For each fix:
   - Read the relevant file to understand context before editing.
   - Make the minimal change that addresses the finding.
   - Do NOT refactor unrelated code or add unrelated improvements.

3. After all fixes are applied, verify your changes do not break anything. Read CLAUDE.md, README.md, package.json, Makefile, or other project config files to discover the project's format, lint, build, and test commands. Then run each available step in order:

   a. **Format**
   b. **Lint**
   c. **Build**
   d. **Test** (prefer unit tests over integration/e2e if distinguishable)

   If a step fails:
   - Determine whether the failure is caused by your changes or is pre-existing (baseline-matching counts as pre-existing by definition).
   - If caused by your changes: fix it and re-run the failing step. **Retry budget: if the same signature fails twice in a row after your fixes, stop retrying. Record it under Unresolved Issues and continue.**
   - If pre-existing: note it and continue to the next step.
   - Do NOT ignore or skip failures without this assessment.

   If no commands are found for a step, skip it.

4. Summarize your output in this format:
   ```
   ## Fixes Applied
   - [file:line] description of fix

   ## Verification
   - Format: {PASS/FAIL/SKIPPED} (command used)
   - Lint: {PASS/FAIL/SKIPPED} (command used)
   - Build: {PASS/FAIL/SKIPPED} (command used)
   - Test: {PASS/FAIL/SKIPPED} (command used)

   ## Unresolved Issues
   - [file:line] description (pre-existing / not caused by this fix)
   ```
