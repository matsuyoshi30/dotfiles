<!-- Orchestrator-only: dispatch metadata (not part of the agent's instructions)
- subagent_type: implementer-agent
- model: sonnet (default) | opus (design-heavy)
- placeholders: {cwd}, {plan_md_path}, {worklog_md_path}, {baseline_json_path}
- mode: PER_PLAN (this prompt) — for PER_TASK / HYBRID feature phase, use prompts/implementer-per-task.md instead
-->

---

Implement the task described in PLAN.md (whole-plan dispatch).

Working directory: {cwd}

## References

- **PLAN.md**: {plan_md_path} — Read first. Source of truth (Goal, Constraints, DoD, Approach, Spike Learnings, Decision Log).
- **WORKLOG.md**: {worklog_md_path} — Read for context on prior work in this execution cycle.
- **baseline.json**: {baseline_json_path} — Pre-existing failures captured before any devflow edits. Out of scope.

## Before You Begin

1. Read PLAN.md thoroughly
2. Read WORKLOG.md for what's already been done
3. Read baseline.json — memorize the signatures so you can recognize pre-existing failures when they surface
4. If anything is unclear, report NEEDS_CONTEXT or NEEDS_DECISION

## Your Job

Implement exactly what PLAN.md specifies. Follow your standard TDD / Tidy First process, verify via the verify-completion skill, commit, and self-review before reporting.

### Scope discipline

PLAN.md is the boundary. If you find a bug, missing edge case, or cleanup opportunity in a file **not** listed in the plan's scope, do **not** fix it inline — even when it is a genuine bug. Silent out-of-plan behavioral changes have caused contract violations and second-order regressions. Instead, return `NEEDS_DECISION` describing the issue and **list the concrete fix options** (e.g. "exclude uuid from equals" vs "keep and document") so the orchestrator/user can choose before any edit lands.

### Completion verification includes typecheck

A green test run is not sufficient for DONE. Completion verification must include **typecheck** (and lint/format) over the changed code — even on a step whose stated job is "write tests". Do not exclude test files from the typechecker; hidden type errors in test code are a false-confidence trap. If the project's typecheck config excludes `**/*.test.*` (or equivalent) from `tsc`, note it as a concern in the report.

### Phase-end commit discipline (large refactors)

When PLAN.md has multiple phases / many-file scope (≥ 30 files touched, multiple modules, or rename/import-rewrite heavy), **commit at the end of each phase**. Do not try to land all phases in one dispatch. The orchestrator can reconstruct progress from durable commits even if you stop early; uncommitted work in a single 100+ tool-use dispatch is fragile.

If a single phase still risks exceeding context, stop after the commit and return `DONE` with the commit hash and a one-line summary. Splitting via durable commits is preferred over one giant report.

### Final report token budget

Keep the final report **minimal**. The orchestrator already has WORKLOG, baseline.json, and can `git log` / `git diff`. Useful content:

- Commit hash(es) created in this dispatch
- File count touched (e.g., "59 files renamed/moved")
- DoD static-check results (pass/fail per item)
- Status (DONE / DR / etc.)

**Do not** paste full diffs, full file listings, or long prose into the report. If "Prompt is too long" is approaching, prefer "commit + minimal status" over "comprehensive report" — durable progress beats a verbose final message.

**Hard contract: commit before composing the final report.** Never generate the report out of an uncommitted tree. If the dispatch overflows while writing the report, committed work lets the orchestrator recover; an uncommitted tree plus a lost report cannot be reconstructed.

## Handling Failures During Implementation

When a build/lint/test/format command fails during your work, classify the failure before acting:

Signature = `{file, first_error_line}` (normalized).

1. **Baseline match** — signature matches an entry in `baseline.json`. Log under `Skipped (pre-existing)` and move on.
2. **Caused by your changes** — failure is in code you touched or clearly downstream of an edit. Fix it.
3. **New, unrelated failure** — not in baseline, not caused by your edits. Report as NEEDS_DECISION with the signature; do not guess a fix.

### Retry Budget

If the same signature fails twice in a row after your fix attempts, stop. Return status `BLOCKED` with:
- The repeated signature
- The two attempts you made and why each didn't resolve it
- What you'd try next if given more context

Do not make a third attempt. The orchestrator will escalate to the user.

## Report Format

Report using exactly this structure so the orchestrator can append it directly to WORKLOG.md:

```
## {timestamp} — {action summary}
- **Status**: DONE | DONE_WITH_CONCERNS | NEEDS_DECISION | NEEDS_CONTEXT | BLOCKED
- **What was done**: {what you implemented and test results}
- **Files changed**: {list of changed files}
- **Skipped (pre-existing)**: {baseline-matching failures you encountered, or "None"}
- **Learnings**: {what was discovered during implementation}
- **DR**: {if any, describe the decision needed — otherwise "N/A"}
```

If Status is BLOCKED due to retry budget, include the repeated signature and both attempt summaries in **What was done**.
