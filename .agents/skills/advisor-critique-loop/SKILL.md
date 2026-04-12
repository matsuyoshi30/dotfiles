---
name: advisor-critique-loop
description: Multi-model Driver/Critic pipeline for requirements → design → plan → implementation with gated reviews.
---

# Advisor Critique Loop

A development pipeline that assigns a **Driver** and a **Critic** (always different models) to each phase, looping until all critique is resolved.

## When to Use

- User asks to build a new feature or project from scratch
- User wants multi-model review on requirements, design, or implementation plan
- **NOT for:** bug fixes, small changes, single-file edits

## Role Matrix

| Phase | Driver | Critic | Artifact |
|---|---|---|---|
| 1. Requirements | Opus | Codex | `01_requirements.md` |
| 2. Design | Codex | Opus | `02_design.md` |
| 3. Implementation Plan | Opus | Codex | `03_impl_plan.md` |
| 4. Implementation | Cursor Composer (fallback: Sonnet) | Codex | source code + `04_impl_notes.md` |

**Rule:** Driver and Critic must always be different models.

## Prerequisites

**Before first use, run `bash scripts/detect_tools.sh`** to verify tools and generate `.advisor/tools.json`.

- **Required:** `claude`, `codex`, `jq`
- **Optional:** `cursor-agent` — falls back to Claude Sonnet for implementation phase

## Workflow

Creates `.advisor/<run-id>/` for all artifacts and review logs.

```
for phase in [requirements, design, impl_plan, implementation]:
    artifact = DRIVER.run(prompt=phase.driver_prompt, context=prev_artifacts)
    validate_structure(artifact, phase)
    iteration = 0
    while True:
        review = CRITIC.run(prompt=phase.critic_prompt, context=artifact)
        if review.verdict == "LGTM" or iteration >= MAX_ITER:
            break
        artifact = DRIVER.run(prompt=phase.fix_prompt, context=artifact + review)
        iteration += 1
    commit(artifact)
```

`MAX_ITER = 4` (default). Escalates to user if exceeded.

## Escalation Conditions

- MAX_ITER exceeded
- Critic raises the same issues two iterations in a row
- Critic returns invalid (non-JSON) output
- CLI exits with non-zero code (`set -euo pipefail`)

On escalation: report to the user and ask for guidance. Never silently compromise.

## Progress Checklist

Copy at the start of each run:

```
Pipeline Progress:
- [ ] Phase 1: Requirements        — Driver(Opus)  → Critic(Codex) → LGTM
- [ ] Phase 2: Design              — Driver(Codex) → Critic(Opus)  → LGTM
- [ ] Phase 3: Implementation Plan — Driver(Opus)  → Critic(Codex) → LGTM
- [ ] Phase 4: Implementation      — Driver(Impl)  → Critic(Codex) → LGTM
- [ ] All artifacts saved to .advisor/<run-id>/
```

## Usage

```bash
$ echo "Build a URL shortener API" > request.md
$ bash scripts/run.sh request.md
```
