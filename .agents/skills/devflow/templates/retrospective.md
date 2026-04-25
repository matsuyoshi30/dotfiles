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
| Implementer dispatches | n |
| DRs raised | n |
| ABORTED_RETRY_LOOP | n |
| Spec review iterations | n / 2 |
| Code quality iterations | n / 3 |
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
- Were retry aborts triggered too early / too late?

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
