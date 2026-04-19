<!-- Orchestrator-only: dispatch metadata (not part of the agent's instructions)
- subagent_type: implementer-agent
- model: sonnet (default) | opus (design-heavy)
- placeholders: {cwd}, {plan_md_path}, {worklog_md_path}
-->

---

Implement the task described in PLAN.md.

Working directory: {cwd}

## References

- **PLAN.md**: {plan_md_path} — Read first. Source of truth (Goal, Constraints, DoD, Approach, Spike Learnings, Decision Log).
- **WORKLOG.md**: {worklog_md_path} — Read for context on prior work in this execution cycle.

## Before You Begin

1. Read PLAN.md thoroughly
2. Read WORKLOG.md for what's already been done
3. If anything is unclear, report NEEDS_CONTEXT or NEEDS_DECISION

## Your Job

Implement exactly what PLAN.md specifies. Follow your standard TDD / Tidy First process, verify via the verify-completion skill, commit, and self-review before reporting.

## Report Format

Report using exactly this structure so the orchestrator can append it directly to WORKLOG.md:

```
## {timestamp} — {action summary}
- **Status**: DONE | DONE_WITH_CONCERNS | NEEDS_DECISION | NEEDS_CONTEXT | BLOCKED
- **What was done**: {what you implemented and test results}
- **Files changed**: {list of changed files}
- **Learnings**: {what was discovered during implementation}
- **DR**: {if any, describe the decision needed — otherwise "N/A"}
```
