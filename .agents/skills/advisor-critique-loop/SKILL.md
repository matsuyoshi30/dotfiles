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
| 4. Implementation | Cursor Composer (fallback: Sonnet) | Codex | source code (written to project root) + `04_impl_notes.md` (under `.advisor/<run-id>/`) |

**Rule:** Driver and Critic must always be different models.
**Fallback scope:** The `cursor-agent` → Claude Sonnet fallback applies to **Phase 4 only**. Phases 1-3 require `claude` (Opus) and `codex`; no fallback.

## Prerequisites

**Before first use, run `bash scripts/detect_tools.sh`** to verify tools and generate `.advisor/tools.json`.

- **Required:** `claude`, `codex`, `jq`
- **Optional:** `cursor-agent` — falls back to Claude Sonnet for implementation phase

**Note:** `detect_tools.sh` only checks PATH presence (`command -v`). It does NOT verify authentication or functional state. Before the first run, manually confirm each Required CLI actually works (e.g. `codex --version` succeeds, `claude` is logged in). If a Required tool is on PATH but broken (auth error, bad config), the pipeline will fail mid-phase — escalate to the user per "Escalation Conditions" rather than retry.

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

`MAX_ITER = 4` (default, **per phase** — each of the 4 phases has its own counter). Escalates to user if exceeded within any phase.

## Escalation Conditions

- **Pre-run**: a Required CLI on PATH is broken (auth error, missing config). Do not start the pipeline — escalate immediately.
- MAX_ITER exceeded (in any phase)
- Critic raises the same issues two iterations in a row — "same" means matching `finding.id` in the critic's JSON output (not string-equality of the message)
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

Run from the **project root** where you want `.advisor/<run-id>/` artifacts to land. `run.sh` writes `.advisor/` relative to the current working directory.

```bash
$ cd <project-root>
$ cat > request.md <<'EOF'
# <Short title>

## Goal
<1-3 sentences: what are we building, why>

## Scope
- In: <bulleted list>
- Out: <explicitly excluded items>

## Constraints
- Language/stack: <e.g. Go + SQLite>
- Non-functional: <perf, security, deployment targets>

## Open questions
- <anything the Phase 1 Requirements driver should resolve>
EOF
$ bash ~/.claude/skills/advisor-critique-loop/scripts/run.sh request.md
```

A free-form single-line request also works (e.g. `echo "Build a URL shortener API" > request.md`), but the structured template above yields higher-quality Phase 1 requirements with fewer critique iterations.
