# Auto Mode (`--auto`)

Auto mode is opt-in via `--auto` (e.g. `/devflow --auto <task>`). It is intended for **lightweight tasks** where the orchestrator's recommended choice at each gate is overwhelmingly likely to be correct, and the cost of being wrong is low. When unset, devflow runs with the normal interactive gates.

## Activation

When `--auto` is present, the orchestrator runs **Step 0.5 (Lightweight Check)** immediately after Step 0 and before Step 1. This gate exists *because* auto mode bypasses later human checkpoints — if the task is not actually lightweight, the user is asked **once, early**, before any subagent dispatch.

## Step 0.5 — Lightweight Check (auto mode only)

Classify the task as `LIGHT` or `NOT_LIGHT` from `task_summary` alone (no codebase reads yet).

- **LIGHT** when ALL of:
  - Estimated step count ≤ 3 (rough mental count from the task description)
  - Single directory or single tight cluster of files
  - Definition of Done is unambiguously derivable from `task_summary` (no DoD-shaped questions remain)
  - No new external dependency / new library / new public API
  - No mention of cross-cutting concerns (auth, schema migration, performance budget, security boundary)
- **NOT_LIGHT** when any of the above is violated.

Display: `> Auto-mode lightweight check: {LIGHT | NOT_LIGHT} — {reason}`

- On `LIGHT`: proceed to Step 1 with auto mode active.
- On `NOT_LIGHT`: present the classification + reason to the user with three options:
  - (A) **Stay auto** — proceed with auto-approval anyway (user accepts the risk).
  - (B) **Switch to interactive** — disable auto, run normal gates from Step 1 onward.
  - (C) **Abort** — stop devflow.

  Wait for user response. Record the choice in WORKLOG.md as `AUTO_LIGHTWEIGHT_CHECK: NOT_LIGHT → {A|B|C} (reason)`.

## Gate behavior in auto mode

When auto mode is active (and the user did not switch to interactive at Step 0.5), the orchestrator **auto-approves its own recommended choice** at the following gates and logs each decision to WORKLOG as `AUTO_APPROVED: {gate} → {choice} (reason)`:

| Gate | Auto behavior |
|------|---------------|
| Step 2 Plan-Refine: DIALOGUE vs DIRECT | Choose DIRECT when criteria are met; otherwise still DIRECT (skip dialogue) but flag that DoD must be self-derivable. |
| Step 2 PLAN.md user approval | Auto-approve **only if** required sections (`## Goal`, `## Definition of Done`, `## Approach`) are concretely filled. Vague / TBD DoD → fall back to interactive (this never auto-approves). |
| Step 3 Spike: RUN vs SKIP | Default SKIP for LIGHT tasks; auto-approve recommendation. |
| Step 3 Spike re-approval (unified diff) | Auto-approve. |
| Step 3 INSUFFICIENT after iter 2 (A/B/C/D) | **Hard guardrail — always escalate to user.** Auto cannot resolve repeated spike insufficiency. |
| Step 4 Isolation gate | Default IN_PLACE for LIGHT tasks; auto-approve recommendation. |
| Step 4 DR (Decision Record) | Auto-pick the recommended option **only when** the recommendation has a clear rationale (one option strictly dominates). When alternatives are close, **hard guardrail — escalate to user**. |
| Step 4 ABORTED_RETRY_LOOP | **Hard guardrail — always escalate to user.** Same failure twice does not become resolvable by auto-progression. |
| Step 5/6 review failures after max iterations | **Hard guardrail — always escalate to user.** |
| Vague / missing `## Definition of Done` | **Hard guardrail — interactive fallback** (never auto-approves). |

Rows tagged **Hard guardrail** apply regardless of `--auto`.

## Mid-task safeguard

If during execution the orchestrator detects the task is heavier than the Step 0.5 classification suggested (e.g., PLAN.md ends up with `## Steps` count ≥ 5, or touches multiple packages), append `AUTO_DRIFT_DETECTED` to WORKLOG and revert to interactive mode for the remaining gates. Do not silently keep auto-approving in this case.
