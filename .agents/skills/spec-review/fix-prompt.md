Align the implementation with its specification.

Working directory: {cwd}

## Specification

{resolved_spec_text}

## Spec Compliance Findings

{review_output}

## Instructions

1. Fix issues in priority order: Missing requirements → Misunderstandings → Extra/unneeded work.
2. For each fix:
   - Read the relevant file to understand context before editing.
   - Make the minimal change that addresses the finding.
   - For "Extra" findings: remove the unneeded code unless removal would break other functionality.
   - Do NOT refactor unrelated code or add unrelated improvements.

3. After all fixes are applied, verify your changes do not break anything. Read CLAUDE.md, README.md, package.json, Makefile, or other project config files to discover the project's format, lint, build, and test commands. Then run each available step in order:

   a. **Format**
   b. **Lint**
   c. **Build**
   d. **Test** (prefer unit tests over integration/e2e if distinguishable)

   If a step fails:
   - Determine whether the failure is caused by your changes or is pre-existing.
   - If caused by your changes: fix it and re-run the failing step. Repeat until it passes.
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
