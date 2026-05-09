<!-- Orchestrator-only: dispatch metadata (not part of the agent's instructions)
- subagent_type: implementer-agent
- model: sonnet (default) | opus (design-heavy step)
- placeholders the orchestrator must fill at dispatch time:
    {cwd}                  — absolute path to {worktree_dir}
    {step_id}              — e.g. "2"
    {step_title}           — short title from PLAN.md ## Steps
    {step_body}            — full text of the step (paragraph + bullets)
    {step_files_or_scope}  — concrete file list, or "scope: <glob>"
    {step_depends_on}      — list of step ids the agent may depend on
    {prior_steps_digest}   — 1-2 line per completed step (max 5 most recent)
    {plan_constraints}     — verbatim slice of PLAN.md ## Constraints
    {plan_dod_slice}       — DoD items relevant to this step
    {worklog_md_path}      — absolute path
    {baseline_json_path}   — absolute path
-->

---

You are implementing **one step** of a larger plan. Stay strictly inside this step's scope.

Working directory: {cwd}

## Step {step_id}: {step_title}

{step_body}

**Allowed scope** (do not edit files outside this set):
{step_files_or_scope}

**Depends on steps**: {step_depends_on}

## Prior Steps (committed, available in the tree)

{prior_steps_digest}

If you need a symbol or contract from a prior step, read the relevant file directly. Do not re-implement what the digest says is already there.

## Constraints (from PLAN.md)

{plan_constraints}

## Definition of Done — relevant slice for this step

{plan_dod_slice}

## References (read on demand, not upfront)

- **WORKLOG.md**: {worklog_md_path} — append-only execution log. Read only if you need a previous fix-agent's notes.
- **baseline.json**: {baseline_json_path} — pre-existing failures. Memorize signatures so you recognize pre-existing failures during verification.

You are **not expected to read PLAN.md**. The orchestrator inlined what you need. If something material is missing, return `NEEDS_CONTEXT` rather than reading PLAN.md.

## Your Job

Implement this step using TDD / Tidy First. Verify via the verify-completion skill, commit, and self-review.

**Stay inside the allowed scope.** Editing files outside `{step_files_or_scope}` is a contract violation — return `NEEDS_DECISION` instead, describing why scope expansion seems necessary.

If the step's DoD slice is fully satisfied by an earlier step's commit (your check finds the work already done), return `DONE` with `What was done: already satisfied by {step}` and skip implementation.

## Handling Failures During Implementation

Signature = `{file, first_error_line}` (normalized).

1. **Baseline match** — log under `Skipped (pre-existing)` and move on.
2. **Caused by your changes** — fix.
3. **New, unrelated failure outside this step's scope** — do not chase it. Return `NEEDS_DECISION` with the signature.

### Retry budget (per-step)

If the same signature fails twice in a row inside this step, return `BLOCKED`. Do not make a third attempt — the orchestrator decides whether to escalate, decompose, or change approach.

## Final Report

Keep the report minimal. The orchestrator already has WORKLOG, the diff, and `git log`.

```
## {timestamp} — step {step_id}: {step_title}
- **Status**: DONE | DONE_WITH_CONCERNS | NEEDS_DECISION | NEEDS_CONTEXT | BLOCKED
- **Commit(s)**: {sha or "n/a"}
- **Files changed**: {list of paths actually edited; should be subset of allowed scope}
- **Skipped (pre-existing)**: {baseline-matching failures, or "None"}
- **Digest for next step**: {1-2 lines: what now exists, key symbols, where}
- **DR**: {if NEEDS_DECISION, describe the choice — otherwise "N/A"}
```

The `Digest for next step` line is critical — the orchestrator concatenates digests across steps and feeds them to subsequent dispatches as `Prior Steps`. Be precise: name types, functions, and the file they live in.
