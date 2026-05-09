# Retrospective

Use at Step 9 (background). retro-agent writes this file to `{devflow_dir}/retrospective.md`.

```markdown
# Retrospective: {task slug}

Date: {YYYY-MM-DD}
Devflow dir: {devflow_dir}
Branch: {branch}

## Summary

{1-3 lines: what was done / outcome / one-line takeaway for next time}

## Analysis

### Devflow Signals
| Signal | Value |
|--------|-------|
| Plan-Refine path | DIALOGUE / DIRECT |
| Spike | RUN / SKIP (reason) |
| Isolation | WORKTREE / IN_PLACE |
| Execution Mode | PER_PLAN / PER_TASK / HYBRID (reason) |
| Implementer dispatches | n (per-step count for PER_TASK) |
| DRs raised | n |
| ABORTED_RETRY_LOOP | n (which step, if PER_TASK) |
| Per-step review iterations (PER_TASK) | total n |
| Spec review iterations (final / full) | n / cap |
| Code quality iterations (final / full) | n / cap |
| Baseline failures (pre-existing) | n |
| Verification: Lint/Build/Test | PASS / FAIL / SKIP |

### What Worked
- {decisions or patterns that paid off}

### What Didn't Work
- {rework, wasted iterations, mis-judgments and root causes}

### Gate Decisions Reviewed
- Was DIALOGUE/DIRECT the right call?
- Was Spike RUN/SKIP the right call?
- Was Isolation WORKTREE/IN_PLACE the right call?
- Was Execution Mode (PER_PLAN/PER_TASK/HYBRID) the right call? Compare actual file_count / step_count from `tuning.jsonl` against the chosen mode's classification rule.
- Were retry aborts triggered too early / too late?

### Tuning Log Trends (read-only)
Read `{base_repo}/.devflow/tuning.jsonl` (recent ~10 entries). Surface patterns that justify a threshold tweak (e.g., "PER_TASK consistently triggers final-review escalation when step_count ≥ 8 — consider lowering the per-step diff cap"). Trend findings feed the `## Improvements` section as proposals.

## Knowledge

### {knowledge title}
- **Context**: {when it came up}
- **Insight**: {what we learned}
- **Next time**: {what to do differently}

(If none: write "特になし".)

## Improvements (proposals only — no auto-apply)

### {proposal title}
- **Target**: {file path + section}
- **Problem**: {cite the WORKLOG entry that motivates this}
- **Proposed change**: {before/after sketch}

(If none: write "特になし".)
```
