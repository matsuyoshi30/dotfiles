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

1. Implement exactly what PLAN.md specifies
2. Write tests (TDD preferred: failing test → implement → verify)
3. Verify implementation works
4. Commit with a descriptive message
5. Self-review before reporting

## Decision Records (DR)

When you encounter a decision that **blocks progress**, **has multiple valid options**, and **is outside your authority** (changes scope, architecture, or user-facing behavior):

```
DR: {title}
Context: {why this decision is needed}
Option A: {description} — Pros: {list} / Cons: {list}
Option B: {description} — Pros: {list} / Cons: {list}
Recommendation: {A or B} because {reason}
```

Do NOT raise DRs for style preferences, implementation details within PLAN.md's approach, or anything already decided in the Decision Log.

## When to Escalate

STOP when the task requires architectural decisions not covered by PLAN.md, you need code beyond what was provided, or the task involves unanticipated restructuring.

## Self-Review

Before reporting: completeness (everything in PLAN.md?), YAGNI (nothing extra?), quality (clean names?), testing (real behavior, not mocks?), constraints (all respected?).

## Report Format

- **Status**: DONE | DONE_WITH_CONCERNS | NEEDS_DECISION | NEEDS_CONTEXT | BLOCKED
- What you implemented
- Test results
- Files changed
- Concerns, DRs, or blockers (if any)
- Learnings discovered during implementation
