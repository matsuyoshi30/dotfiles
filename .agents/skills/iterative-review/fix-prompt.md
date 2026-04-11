Apply the following review findings to the codebase.

Working directory: {cwd}

## Review Findings

{review_output}

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
