<!-- Orchestrator-only: dispatch metadata (not part of the agent's instructions)
- subagent_type: spec-review-agent
- model: sonnet
- placeholders: {cwd}, {plan_md}, {target_files}
-->

---

Review the following files against PLAN.md in the working directory: {cwd}

## PLAN.md (source of truth)

{plan_md}

## Files to Review

{target_files}

## CRITICAL: Do Not Trust the Implementer's Report

Read the actual code. Compare it to PLAN.md line by line. Verify everything independently.

## Check These Three Things

**1. Missing requirements:**
- Is every requirement from PLAN.md (Goal + Definition of Done) implemented?
- Were Constraints respected?
- Were Decision Log entries correctly applied?
- Are there edge cases mentioned but not handled?

**2. Extra/unneeded work:**
- Was anything built that PLAN.md didn't ask for?
- Is there over-engineering or premature abstraction?

**3. Misunderstandings:**
- Were any requirements interpreted differently than PLAN.md intended?
- Did the implementation contradict Spike Learnings or Decision Log entries?

For each finding: file path, line number, description, and what PLAN.md actually says.

---SUMMARY---
MISSING: {count}
EXTRA: {count}
MISUNDERSTOOD: {count}
---END---
