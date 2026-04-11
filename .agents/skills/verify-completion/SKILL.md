---
name: verify-completion
description: Verification gate before claiming work is complete. Auto-discovers and runs test/lint/build commands, checks exit codes and output, and blocks completion claims without fresh evidence. Use before committing, creating PRs, or claiming a task is done.
allowed-tools: Bash, Read, Glob, Grep
user-invocable: true
---

# Verification Before Completion

## The Iron Law

```
NO COMPLETION CLAIMS WITHOUT FRESH VERIFICATION EVIDENCE
```

If you haven't run the verification command in this response, you cannot claim it passes.

## Step 1 — Discover Verification Commands

Auto-detect available commands by checking these sources in order:

### Lint
1. `CLAUDE.md` or `README.md` for documented lint commands
2. `package.json` scripts: `lint`, `check`, `format:check`
3. `Makefile` / `Justfile`: `lint`, `check` targets
4. Config files: `.eslintrc*`, `pyproject.toml` (`[tool.ruff]`, `[tool.flake8]`), `rustfmt.toml`
5. Language defaults: `go vet ./...`, `cargo clippy`, `swift build` (warnings)

### Tests
1. `CLAUDE.md` or `README.md` for documented test commands
2. `package.json` scripts: `test`, `test:unit`
3. `Makefile` / `Justfile`: `test` target
4. Language defaults: `go test ./...`, `pytest`, `cargo test`, `swift test`
5. Prefer unit tests over integration/e2e if distinguishable

### Build
1. `package.json` scripts: `build`, `compile`
2. `Makefile` / `Justfile`: `build` target
3. Language defaults: `go build ./...`, `cargo build`, `tsc --noEmit`

Report which commands were found and which were not discoverable.

## Step 2 — Run All Discovered Commands

For each discovered command:

1. **Run** the full command (no partial runs, no cached results)
2. **Capture** exit code and output
3. **Parse** output for failure indicators:
   - Exit code != 0
   - "FAIL", "ERROR", "error:", failure count > 0
   - Warnings count (report but don't block)

## Step 3 — Report Results

```markdown
## Verification Results

| Check | Command | Exit Code | Result |
|-------|---------|-----------|--------|
| Lint  | {cmd}   | {code}    | PASS / FAIL / SKIP (not found) |
| Test  | {cmd}   | {code}    | PASS ({n} passed) / FAIL ({n} failed) / SKIP |
| Build | {cmd}   | {code}    | PASS / FAIL / SKIP |

**Verdict**: {ALL PASS | FAILURES FOUND}
```

If any check fails:
- Report the specific failures
- Do NOT claim work is complete
- Suggest fixes if the cause is apparent

If all checks pass:
- Report with evidence (exit codes, pass counts)
- Work may be claimed as complete

## Red Flags — STOP

These phrases are BANNED without fresh verification evidence:

- "should pass", "probably works", "seems correct"
- "I'm confident", "looks good"
- "tests passed earlier", "based on the previous run"
- Any variation of completion/success claims without Step 2 output

## When To Use

- Before committing
- Before creating PRs
- Before claiming a task is done
- Before moving to the next task
- When another skill or workflow asks for completion verification

## Important Rules

- **Fresh runs only.** Previous run results are not evidence.
- **Full commands only.** Running a subset of tests is not verification.
- **Exit codes matter.** Read them, don't assume.
- **Parse output.** Exit code 0 with "1 failed" in output is still a failure.
- **Skip gracefully.** If no command is discoverable for a check, report SKIP — don't guess.
