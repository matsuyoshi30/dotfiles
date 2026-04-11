---
name: devflow
description: End-to-end development workflow that orchestrates implementation, spec compliance review, code quality review, and completion verification using typed subagents with model selection by task complexity. Use when you have a clear task or spec and want autonomous implementation with quality gates.
allowed-tools: Agent(implementer-agent, spec-review-agent, review-agent, fix-agent), Bash, Read, Glob, Grep
user-invocable: true
---

# Development Flow

Autonomous development workflow: implement → spec compliance review → code quality review → completion verification. Uses typed subagents with dedicated prompt templates to preserve orchestrator context, with model selection by task complexity.

## Parameters

- **Task** (required argument): What to implement. Accepts:
  - GitHub issue URL
  - File path to spec/design document
  - Inline description of the task
- **Target** (optional): Working directory or specific files. Defaults to cwd.

## Overview

```
[Resolve task/spec]
    ↓
[Implement] ← implementer-agent (model by complexity)
    ├─ DONE → continue
    ├─ DONE_WITH_CONCERNS → assess, then continue
    ├─ NEEDS_CONTEXT → provide context, re-dispatch
    └─ BLOCKED → escalate or re-dispatch with stronger model
    ↓
[Spec Compliance Review] ← spec-review-agent (model: sonnet)
    loop: review → fix-agent → re-review (max 2)
    ├─ ✅ Compliant → continue
    └─ ❌ Issues remain → report to user, stop
    ↓
[Code Quality Review] ← review-agent (model: opus, reviewing-code preloaded)
    loop: review → fix-agent → re-review (max 3)
    ├─ ✅ Approved (Critical/High = 0) → continue
    └─ ❌ Critical/High remain → report to user, stop
    ↓
[Completion Verification] ← inline (not subagent)
    ├─ ALL PASS → final report
    └─ FAILURES → report, do not claim completion
```

## Step 0 — Resolve Task

Parse the task argument:

- **GitHub issue URL**: `gh issue view <url> --json title,body,labels`
- **File path**: Read the file
- **Inline text**: Use as-is

Store the resolved spec text — it will be passed to both implementer and reviewers.

## Step 1 — Implement

### Model Selection

Assess task complexity and select model:

| Signal | Model | Examples |
|--------|-------|---------|
| 1-2 files, clear spec, mechanical | `haiku` | Add a field, rename, write a test |
| Multi-file, integration, pattern matching | `sonnet` | Wire up a new endpoint, add a feature |
| Architecture, design judgment, ambiguous | `opus` | Restructure a module, complex business logic |

### Dispatch Implementer Subagent

Read [implementer-prompt.md](implementer-prompt.md) and fill in the placeholders:
- `{resolved_spec_text}` — the full task specification
- `{cwd}` — working directory
- `{context}` — any additional codebase context

Launch an Agent with `subagent_type: "implementer-agent"` and `model: {selected_model}`.

### Handle Implementer Status

- **DONE**: Proceed to Step 2.
- **DONE_WITH_CONCERNS**: Read concerns. Correctness/scope issue → address before review. Observation → note and proceed.
- **NEEDS_CONTEXT**: Provide missing info, re-dispatch same model.
- **BLOCKED**: Assess blocker:
  1. Context problem → provide more context, re-dispatch
  2. Needs more reasoning → re-dispatch with `opus`
  3. Task too large → break into subtasks, implement sequentially
  4. Spec itself is wrong → escalate to user

**Never** retry the same model with no changes. If it's stuck, something needs to change.

## Step 2 — Spec Compliance Review Loop (max 2 iterations)

### 2a. Dispatch Spec Reviewer Subagent

Read [spec-reviewer-prompt.md](spec-reviewer-prompt.md) and fill in:
- `{cwd}` — working directory
- `{resolved_spec_text}` — the specification
- `{target_files}` — files changed by implementer

Launch an Agent with `subagent_type: "spec-review-agent"` and `model: "sonnet"`.

### 2b. Check Exit Condition

Parse the `---SUMMARY---` block.

- If **MISSING = 0 AND EXTRA = 0 AND MISUNDERSTOOD = 0**: proceed to Step 3.
- Otherwise: dispatch fix subagent.

### 2c. Dispatch Fix Subagent

Read [fix-prompt.md](fix-prompt.md) and fill in placeholders with spec and review findings.

Launch an Agent with `subagent_type: "fix-agent"` and `model: "sonnet"`.

### 2d. Next Iteration

Go back to 2a. If max iterations reached with issues remaining: report to user and stop.

## Step 3 — Code Quality Review Loop (max 3 iterations)

### 3a. Dispatch Code Quality Reviewer Subagent

Read [code-quality-reviewer-prompt.md](code-quality-reviewer-prompt.md) and fill in:
- `{cwd}` — working directory
- `{target_files}` — files changed
- `{what_was_implemented}` — summary from implementer
- `{base_sha}` / `{head_sha}` — git commit range

Launch an Agent with `subagent_type: "review-agent"` and `model: "opus"`.

The `review-agent` has the `reviewing-code` skill preloaded via its agent definition.

### 3b. Check Exit Condition

Parse the `---SUMMARY---` block.

- If **CRITICAL = 0 AND HIGH = 0**: proceed to Step 4. Medium/Low are reported but don't block.
- Otherwise: dispatch fix subagent.

### 3c. Dispatch Fix Subagent

Read [fix-prompt.md](fix-prompt.md) and fill in with review findings.

Launch an Agent with `subagent_type: "fix-agent"` and `model: "sonnet"`.
Fix in priority order: Critical → High. Medium/Low are not fixed in this loop.

### 3d. Next Iteration

Go back to 3a. If max iterations reached with Critical/High remaining: report to user and stop.

## Step 4 — Completion Verification

Invoke the verify-completion skill by calling the Skill tool with skill: "verify-completion".

If the skill reports failures: do NOT claim completion.

## Step 5 — Final Report

```markdown
## Development Flow Complete

**Task**: {task summary}

### Implementation
- **Model used**: {model}
- **Files changed**: {list}
- **Tests**: {pass count}

### Spec Compliance
- **Iterations**: {n} / 2
- **Result**: {Compliant | Issues remain}

### Code Quality
- **Iterations**: {n} / 3
- **Result**: {Clean | Issues remain}
- **Remaining**: {Medium/Low findings if any}

### Verification
| Check | Result |
|-------|--------|
| Lint  | {PASS/FAIL/SKIP} |
| Test  | {PASS/FAIL/SKIP} |
| Build | {PASS/FAIL/SKIP} |

**Verdict**: {COMPLETE | ISSUES REMAIN — see above}
```

## Model Selection Summary

| Role | Subagent Type | Model | Why |
|------|---------------|-------|-----|
| Mechanical implementation | implementer-agent | haiku | Fast, low cost |
| Integration implementation | implementer-agent | sonnet | Balance |
| Design-heavy implementation | implementer-agent | opus | Strong reasoning |
| Spec compliance review | spec-review-agent | sonnet | Criteria-based comparison |
| Code quality review | review-agent | opus | Holistic judgment |
| Fix | fix-agent | sonnet | Directed fixes |

## Important Rules

- **Use ONLY the designated subagent types.** implementer-agent, spec-review-agent, review-agent, fix-agent.
- **Spec compliance before code quality.** Wrong thing built well is still wrong.
- **Do not skip review stages.** Even for "trivial" tasks.
- **Subagents are independent.** Each gets fresh context via templates — no shared state.
- **Model selection is deliberate.** Match model to task complexity.
- **Verification is inline.** Evidence must be in orchestrator context.
- **Escalate, don't force.** If implementer is blocked, change approach.
- **One task at a time.** Don't parallelize implementation subagents.
