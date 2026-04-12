---
name: devflow
description: End-to-end development workflow that orchestrates plan refinement (user dialogue), spike prototyping, implementation with WORKLOG/DR loop, and multi-stage review. Use when the user has a clear task or spec and wants autonomous implementation with quality gates.
allowed-tools: Agent(implementer-agent, spec-review-agent, review-agent, fix-agent, spike-review-agent, Explore), Bash, Read, Write, Edit, Glob, Grep
user-invocable: true
---

# Development Flow

Plan-Refine → Plan-Spike → Plan-Execute → Review → Verify.

- **PLAN.md** — single source of truth. All agents read it; only the orchestrator updates it.
- **WORKLOG.md** — append-only execution log. Agents report; orchestrator appends.
- **DR (Decision Record)** — blocker-only A/B choices that bridge agent autonomy and human judgment.

## Overview

```
[Step 0: Resolve Task]
    ↓
[Step 1: Plan-Refine] ← orchestrator + user dialogue → PLAN.md
    ↓
[Step 2: Plan-Spike] ← spike agent (worktree) + spike reviewer → PLAN.md updated
    ↓
[Step 3: Plan-Execute] ← implementer loop → WORKLOG.md + DR → PLAN.md updated
    ↓
[Step 4: Spec Compliance Review] → [Step 5: Code Quality Review]
    ↓
[Step 6: Verification] → [Step 7: Final Report]
```

## Working Directory

Create `{cwd}/.devflow/{YYYY-MM-DDTHH-MM-SS}_{task-slug}/` with PLAN.md and WORKLOG.md. Use [templates/plan.md](templates/plan.md) and [templates/worklog.md](templates/worklog.md) as starting points. Store this path as `{devflow_dir}`.

---

## Step 0 — Resolve Task

- **GitHub issue URL**: `gh issue view <url> --json title,body,labels`
- **File path**: Read the file
- **Inline text**: Use as-is

---

## Step 1 — Plan-Refine (Interactive)

Performed by the orchestrator, NOT a subagent.

1. **Explore context** — scan project structure and relevant files (skip for new projects)
2. **Clarify** — ask questions **one at a time**, prefer **multiple choice**, 3-5 questions typically suffice
3. **Propose approaches** — present **2-3 options** with trade-offs, lead with your recommendation
4. **Write PLAN.md** — fill [templates/plan.md](templates/plan.md), present to user for approval

**Gate:** User must approve PLAN.md before proceeding.

---

## Step 2 — Plan-Spike (Isolated Prototype)

### Assess Need

- **SKIP** when: purely mechanical task, well-established approach with clear precedents, PLAN.md already highly specific
- **RUN** when (default): unfamiliar technology, unknowns, complex existing code, PLAN.md specificity uncertain

Display: `> Spike: {RUN | SKIP} — {reason}`

### Execute

1. **Investigate** (if existing code): dispatch [prompts/investigation.md](prompts/investigation.md) with `thoroughness: "very thorough"`
2. **Prototype**: dispatch [prompts/spike.md](prompts/spike.md) with `isolation: "worktree"` — code is auto-discarded
3. **Update PLAN.md**: extract learnings into `## Spike Learnings`
4. **Review sufficiency**: dispatch [prompts/spike-review.md](prompts/spike-review.md) — context-free, sees only PLAN.md
5. **Refine if needed**: fix gaps, re-run review (max 2 iterations). Unresolved gaps → ask user.

**Gate:** PLAN.md must pass spike review before proceeding.

---

## Step 3 — Plan-Execute (Implementation Loop)

Initialize WORKLOG.md from [templates/worklog.md](templates/worklog.md).

### Dispatch

Read and dispatch [prompts/implementer.md](prompts/implementer.md). See **Model Selection** at bottom.

### Handle Status

| Status | Action |
|--------|--------|
| DONE | Append log → Step 4 |
| DONE_WITH_CONCERNS | Append log → assess: correctness issue → address; observation → proceed |
| NEEDS_DECISION | Handle DR (below) → re-dispatch |
| NEEDS_CONTEXT | Append log → provide info → re-dispatch |
| BLOCKED | Append log → escalate (context / stronger model / decompose / ask user) |

### DR Handling

1. Present the DR to the user as-is — user picks an option
2. Append DR + decision to PLAN.md `## Decision Log`
3. Append to WORKLOG.md
4. Re-dispatch implementer with updated PLAN.md

Never retry the same model with no changes.

---

## Step 4 — Spec Compliance Review (max 2 iterations)

1. Dispatch [prompts/spec-reviewer.md](prompts/spec-reviewer.md)
2. Parse `---SUMMARY---`: if MISSING + EXTRA + MISUNDERSTOOD = 0 → Step 5
3. Otherwise: dispatch [prompts/fix.md](prompts/fix.md), loop back to 1

If issues remain after max iterations: report to user and stop.

---

## Step 5 — Code Quality Review (max 3 iterations)

1. Dispatch [prompts/code-quality-reviewer.md](prompts/code-quality-reviewer.md)
2. Parse `---SUMMARY---`: if CRITICAL + HIGH = 0 → Step 6. Medium/Low reported but don't block.
3. Otherwise: dispatch [prompts/fix.md](prompts/fix.md) (Critical → High priority), loop back to 1

If Critical/High remain after max iterations: report to user and stop.

---

## Step 6 — Completion Verification

Run the project's verification commands in order. Read CLAUDE.md, README.md, package.json, Makefile, or other project config files to discover available commands.

1. **Format** — run formatter if available
2. **Lint** — run linter if available
3. **Build** — run build if available
4. **Test** — run tests if available (prefer unit tests over integration/e2e)

Skip any step with no discoverable command. If a step fails, determine whether the failure is caused by devflow changes or is pre-existing. Only devflow-caused failures block completion.

If any devflow-caused failure remains: do NOT claim completion.

---

## Step 7 — Final Report

```markdown
## Development Flow Complete

**Task**: {task summary}

### Plan-Refine
- **Approach selected**: {brief description}

### Spike
- **Result**: {Performed | Skipped — reason}
- **Key learnings**: {brief summary or "N/A"}

### Implementation
- **Model used**: {model}
- **Files changed**: {list}
- **DRs raised**: {count}
- **Tests**: {pass count}

### Reviews
- **Spec compliance**: {Compliant | Issues remain} (iteration {n}/2)
- **Code quality**: {Clean | Issues remain} (iteration {n}/3)

### Verification
| Check | Result |
|-------|--------|
| Lint  | {PASS/FAIL/SKIP} |
| Test  | {PASS/FAIL/SKIP} |
| Build | {PASS/FAIL/SKIP} |

**Verdict**: {COMPLETE | ISSUES REMAIN}
```

Append to WORKLOG.md.

---

## Model Selection

| Role | Subagent Type | Model |
|------|---------------|-------|
| Codebase investigation | Explore | sonnet |
| Spike implementation | implementer-agent | sonnet |
| Spike review | spike-review-agent | sonnet |
| Standard implementation | implementer-agent | sonnet |
| Design-heavy implementation | implementer-agent | opus |
| Spec compliance review | spec-review-agent | sonnet |
| Code quality review | review-agent | opus |
| Fix | fix-agent | sonnet |

## Rules

- **Plan-Refine is interactive** — orchestrator conducts dialogue, not a subagent
- **Spike code is disposable** — run in worktree, extract learnings, discard code
- **Spike review is context-free** — reviewer sees only PLAN.md
- **DRs are blockers only** — style/preference choices are made autonomously
- **Spec compliance before code quality** — wrong thing built well is still wrong
- **Do not skip review stages**
- **One task at a time** — don't parallelize implementation subagents
