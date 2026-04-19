# Review Iteration Log Entry

Use after each review iteration in Step 5 (Spec) and Step 6 (Code quality). Append to WORKLOG.md.

```
## {timestamp} — {Spec compliance review | Code quality review} (iteration {n})
- **Status**: {DONE if pass, DONE_WITH_CONCERNS if issues fixed, BLOCKED if max iterations}
- **What was done**: {issue counts by severity, fixes applied if any}
- **Files changed**: {list from fix agent, or "(none)" if review only}
- **Learnings**: {notable findings}
- **DR**: N/A
```

## Issue-count conventions

- **Spec compliance review** — report MISSING / EXTRA / MISUNDERSTOOD counts
- **Code quality review** — report CRITICAL / HIGH / MEDIUM / LOW counts
