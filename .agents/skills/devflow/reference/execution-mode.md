# Execution Mode (`PER_PLAN` / `PER_TASK` / `HYBRID`)

Decides how Step 4 dispatches the implementer-agent. Set once in the Step 4 preamble (right after the Isolation gate), and drives Step 5/6 review granularity.

## Modes

| Mode | Dispatch unit | Choose when |
|---|---|---|
| `PER_PLAN` | One implementer-agent for the whole PLAN | step count ≤ 3, or steps tightly coupled (cross-cutting rename, shared types evolve across steps), or docs/config only |
| `PER_TASK` | Fresh implementer-agent per `## Steps` entry | step count ≥ 5, each step roughly independent, each step ≤ 5 files (or `scope:` bounded) |
| `HYBRID` | Foundation phase as `PER_PLAN`, then feature phase as `PER_TASK` | foundation layer (types/config/utilities) followed by independent feature steps that build on it |

When in doubt: `HYBRID` with foundation = first 1-2 steps. It degrades gracefully — to `PER_TASK` if foundation ends up empty, to `PER_PLAN` if every step lands in foundation.

## Classification

Inspect PLAN.md `## Steps` metadata (format defined in `templates/plan.md`):

1. **Step count**: ≤ 3 → propose `PER_PLAN`.
2. **Files / scope**:
   - All steps have concrete `files:` and ≤ 5 files each → strong PER_TASK signal.
   - Mix of `files:` and `scope:` is fine for PER_TASK.
   - Any `files: TBD` → that step cannot run PER_TASK. Either fold into a PER_PLAN block (→ HYBRID) or keep whole plan PER_PLAN.
3. **`depends_on:` graph**:
   - Single chain (1 → 2 → 3 → 4) → per-plan-shaped.
   - Fan-out from a foundation step (1 is shared, 2/3/4 each depend only on 1) → HYBRID-shaped.
   - All independent (no `depends_on`) → PER_TASK-shaped.
4. **Tight coupling override**: if any step would mutate signatures used by other steps, or all steps touch the same handful of files, classify as `PER_PLAN` regardless of step count.
5. **Dispatch budget**: estimate whether a single dispatch can finish before context overflow, from PLAN size — total `## Steps` body length and per-step file counts, not just step count. A large PLAN (rough heuristic: detailed Steps body well over ~6k chars, or any step touching many files / generated code) run as one `PER_PLAN` dispatch repeatedly overflows before the final commit/report. When the budget looks tight, prefer splitting — `PER_TASK`, or `HYBRID` with phase-end commits — over one giant `PER_PLAN` dispatch, even if the coupling signals alone would suggest `PER_PLAN`. Record the budget call in the gate reason.
6. **TDD pre/post-observation override**: if the plan requires observing a test fail *before* a fix and pass *after*, inside the same plan (red→green in one flow), prefer `PER_PLAN` regardless of step count so the pre-fix and post-fix observations stay in a single agent's context. Splitting them across fresh per-step dispatches loses the "Case-A-fails-pre-fix → Case-A-passes-post-fix" evidence.

Signals 4–6 can conflict (e.g. a large TDD-coupled plan). When they do, the budget signal wins for safety: split with phase-end commits and keep the coupled red→green pair within a single phase/dispatch.

## Step 4 preamble — Execution Mode gate

Run after the Isolation gate, before worktree creation:

```
> Execution mode: {PER_PLAN | PER_TASK | HYBRID} — {reason}
```

For `HYBRID`, also display the phase split inferred from `depends_on`:

```
> HYBRID phases:
>   Foundation (PER_PLAN): [step1, step2]
>   Features (PER_TASK):   [step3, step4, step5, step6]
```

Wait for user confirmation. Record in WORKLOG.md as `EXECUTION_MODE: {mode} — {reason}` (HYBRID also lists phases).

**Single-step collapse**: when PLAN.md `## Steps` count = 1, mode is forced to `PER_PLAN`; display as a single line (`> Execution mode: PER_PLAN — single-step plan, gate auto-collapsed`) and proceed without a separate confirmation prompt. Record in WORKLOG as `EXECUTION_MODE: PER_PLAN (single-step collapse)`. This collapse is mirrored in SKILL.md Step 4 preamble.

In auto mode, the gate is auto-approved when classification is unambiguous (see `reference/auto-mode.md`).

## Per-mode dispatch contract

### PER_PLAN

Single dispatch using `prompts/implementer.md`. Implementer reads PLAN.md and works through all steps in one session. Step 5/6 reviews run on the whole diff (existing 2 / 3 iteration caps).

### PER_TASK

For each step in `## Steps` (in order):

1. **Curate context** (orchestrator):
   - The step's full text from PLAN.md `## Steps` (title, files/scope, depends_on, body).
   - 1-2 line digest of completed steps. Example: `step 1 done: defined Issue, Workflow, Run in internal/model/ (commit abc1234)`.
   - Relevant slices of PLAN.md `## Constraints` and `## Definition of Done`.
2. **Dispatch fresh implementer-agent** using `prompts/implementer-per-task.md`. Inline the curated context — the agent does **not** read PLAN.md top-to-bottom.
3. On `DONE` → run **per-step review** (see "Two-tier review" below).
4. On per-step review pass → mark step complete in TodoWrite, append a fresh digest entry to the running handoff state, move to next step.
5. On `NEEDS_DECISION` / `NEEDS_CONTEXT` / `BLOCKED` → handle as in Step 4, scoped to the current step.

After all steps complete and per-step reviews pass: run **final cross-step review** (see below).

### HYBRID

1. Foundation phase: dispatch as `PER_PLAN` over the foundation step set. Run Step 5/6 (per-step variant scoped to the foundation diff, 1 iteration each).
2. Feature phase: dispatch as `PER_TASK` over the feature step set, with per-step + final cross-step reviews.

## Two-tier review (PER_TASK / HYBRID feature phase)

**Per-step review** — after each implementer dispatch returns DONE:

- Run `prompts/spec-reviewer.md` then `prompts/code-quality-reviewer.md` against the **per-step diff only**: `git diff {step_start_sha}..HEAD -- {step_files_or_scope}`.
- Max 2 iterations per reviewer (lower than PER_PLAN's 2 / 3 because the diff is small).
- Findings are tagged with the step id in WORKLOG.

**Final cross-step review** — after all steps land:

- One spec-reviewer pass over the full PLAN-vs-final-diff to catch cross-step gaps (DoD items spanning steps, integration boundaries).
- One code-quality-reviewer pass over the full diff to catch cross-cutting issues (consistency, layering, dead glue).
- Max 1 iteration each. If either still has Critical/High after that single iteration, escalate to user — do not loop.

## Retry budget (per-mode)

| Mode | Scope of `ABORTED_RETRY_LOOP` |
|---|---|
| `PER_PLAN` | Whole-plan retry budget (existing rule: same signature in two consecutive entries → abort). |
| `PER_TASK` | Per-step. Same signature twice in step N triggers ABORT for step N only. Remaining steps continue if their `depends_on` does not include N. If they do, escalate to user. |
| `HYBRID` | Foundation phase: PER_PLAN rules. Feature phase: PER_TASK rules. |

## Handoff digest format

After each PER_TASK step completes (DONE + per-step review pass), append one entry to an in-memory `handoff` state, formatted as:

```
step {id} ({title}): {1-2 line summary of what now exists in the tree, with key symbol names + commit sha}
```

Pass the cumulative handoff (most recent 5 entries is plenty) into the next implementer-per-task dispatch under `## Prior Steps`. Do **not** pass full WORKLOG entries — the digest is curated; WORKLOG is the durable archive.

## Tuning log

After Step 8 final report, the orchestrator appends one JSON object as a single line to `{base_repo}/.devflow/tuning.jsonl` (create the file if missing):

```json
{"date":"2026-05-09","slug":"add-csv-export","step_count":4,"file_count":7,"max_step_files":3,"mode":"PER_TASK","total_minutes":18,"per_step_review_iterations_total":3,"final_review_iterations_total":1,"regressions":0,"baseline_failures_skipped":2,"verdict":"GREEN"}
```

Field meanings:

- `mode`: `PER_PLAN` / `PER_TASK` / `HYBRID`
- `step_count`: count of `## Steps` entries
- `file_count`: total files touched (`git diff --name-only base..HEAD | wc -l`)
- `max_step_files`: largest single-step file count (PER_PLAN: equals `file_count`)
- `per_step_review_iterations_total`: sum of per-step review iterations across all steps (0 for PER_PLAN)
- `final_review_iterations_total`: 1 if final review passed first pass, else escalation count
- `regressions`: count of `DONE_WITH_CONCERNS` or fix-introduced new failures recorded in WORKLOG
- `baseline_failures_skipped`: count of `SKIPPED_PRE_EXISTING` entries
- `verdict`: `GREEN` (Step 7 clean) / `YELLOW` (passed with concerns) / `RED` (escalated to user)

The file is project-local — each repo accumulates its own signal. Retro-agent reads recent entries (read-only) and may surface threshold tweaks under `retrospective.md` `## Improvements`. **Threshold changes to SKILL.md are proposals only — never auto-applied.**

## Mid-task drift safeguard

If `EXECUTION_MODE: PER_PLAN` was set but mid-flight metrics push the run into PER_TASK territory (e.g., the implementer reports it has touched ≥ 10 files across multiple packages, or PLAN.md gets revised mid-execution to ≥ 5 steps), append `EXECUTION_MODE_DRIFT_DETECTED` to WORKLOG and ask the user whether to continue PER_PLAN or pause and re-classify. Do not silently switch modes.
