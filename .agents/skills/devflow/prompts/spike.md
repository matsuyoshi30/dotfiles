<!-- Orchestrator-only: dispatch metadata (not part of the agent's instructions)
- subagent_type: implementer-agent
- model: sonnet
- isolation: worktree
- placeholders: {cwd}, {plan_md}, {context}
-->

---

You are running a **spike** — a throwaway prototype to validate assumptions. Your code will be discarded. Focus on learning, not polish.

Working directory: {cwd}

## PLAN.md

{plan_md}

## Codebase Context

{context}

## Your Job

1. Prototype the approach in PLAN.md — implement enough to validate feasibility
2. Discover unknowns — what's harder or easier than expected?
3. Skip tests, error handling, edge cases. The goal is to learn fast.
4. Do NOT commit — this code will be discarded

## What to Explore

- Does the chosen approach work end-to-end?
- Missing dependencies, version conflicts, compatibility issues?
- Tricky parts that PLAN.md doesn't anticipate?
- Existing codebase patterns the implementation should follow?

## Report Format

```
## Spike Results

### Feasibility
- Does the approach work? {YES / PARTIALLY / NO}
- If not fully: what specifically doesn't work?

### Discoveries
- {What you learned that wasn't in PLAN.md}

### Unexpected Complexities
- {Things harder than PLAN.md suggests}

### Validated Patterns
- {Specific code patterns, APIs, or approaches that work well}
  - Include file paths and line references where applicable

### PLAN.md Gaps
- {What's missing or unclear that would trip up the real implementer}

### Recommended Approach Adjustments
- {Specific changes to the approach based on what you learned}
```
