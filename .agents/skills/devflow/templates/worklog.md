# WORKLOG

Execution log for: {task_summary}
Started: {timestamp}

---

## Entry Format

```
## {timestamp} — {action summary}
- **Status**: {DONE | DONE_WITH_CONCERNS | NEEDS_DECISION | NEEDS_CONTEXT | BLOCKED}
- **What was done**: {summary}
- **Files changed**: {list}
- **Skipped (pre-existing)**: {baseline-matching failures encountered, or "None"}
- **Learnings**: {what was discovered}
- **DR**: {if any, reference to Decision Log entry in PLAN.md — otherwise "N/A"}
```

## Orchestrator Tags

In addition to implementer/fix/review entries, the orchestrator writes short-form entries for decisions and safety-net events:

- `BASELINE_CAPTURED` — pre-flight baseline captured at Step 4 preamble. Body: PASS/FAIL counts per command, unique signature count.
- `SKIPPED_PRE_EXISTING` — a failure matching a baseline signature was encountered and deliberately not fixed. Body: signature + which step.
- `ABORTED_RETRY_LOOP` — retry budget triggered (same signature twice in a row). Body: signature + the two attempts. Requires user escalation.

---
