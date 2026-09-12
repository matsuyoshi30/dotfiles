---
name: shaping-spec
description: Turn a vague idea about an existing codebase into a concise downstream-ready specification without choosing an implementation plan or writing production code.
# `allowed-tools` and `user-invocable` are Claude Code harness extensions
# beyond Anthropic's required name/description. Other harnesses ignore them.
allowed-tools: Read, Write, Edit, Glob, Grep, Bash, Agent(Explore), Skill(devflow, superpowers:writing-plans)
---

# Shaping Spec

Bridge from "I have a rough idea about this codebase" to "here is a spec a downstream skill can act on". The output is deliberately neutral: it records the goal and constraints without turning them into an implementation plan.

## Positioning vs Adjacent Skills

- **superpowers:brainstorming** — general design-doc dialogue, not codebase-aware; use when there is no existing codebase to ground the idea against.
- **devflow** — autonomous end-to-end implementation; expects a clear, codebase-grounded spec or plan as input. Use when the work is ready to execute.
- **shaping-spec (this skill)** — codebase-aware shaping of a vague idea into a downstream-ready spec. Use when the idea is too vague to hand to an implementation-bound skill.

## Output Location

The generated spec file is written to `{base_dir}/specs/{YYYY-MM-DDTHH-MM-SS}_{slug}.md`.

Resolve `{project_root}` from the codebase being shaped, which may differ from the process working directory. Resolve `{base_dir}` in this order:

1. Read `{project_root}/AGENTS.md`, `{project_root}/CLAUDE.md`, `~/.claude/CLAUDE.md`, and one level of their `@filename.md` imports when present. In textual order, collect path tokens from lines that mention notes, specs, work, save, output, location, 保存, or ディレクトリ. Expand `~` and `$HOME`; use the first candidate that exists as a directory.
2. Use the first existing conventional directory: `{project_root}/.matsuyoshi/`, `{project_root}/.matsuyoshi30/`, `~/.matsuyoshi/`, `~/.matsuyoshi30/`.
3. If none exists, create and use `~/.shaping-spec/`.

When one line lists multiple paths, preserve their textual order. Do not reorder them to match another skill's preference.

`{slug}` is a short kebab-case identifier derived from the user's idea (max ~50 chars). Choose it without a separate confirmation unless the name itself affects the requested outcome.

`{YYYY-MM-DDTHH-MM-SS}` uses local time, e.g. `2026-04-21T15-30-42`. Uniqueness at 1-second granularity is assumed — if the write somehow collides, re-run with a fresh timestamp.

## Flow

At the start, register this checklist via TodoWrite and tick items off as you go:

```
shaping-spec progress:
- [ ] Step 0: Receive rough idea + summarise back
- [ ] Step 1: Light exploration (escalate to Explore subagent if criteria met)
- [ ] Step 2: Ground-truth confirmation
- [ ] Step 3: Shaping dialogue (Goal / Constraints / DoD; opt-in optionals)
- [ ] Step 4: Slug propose → write spec → user approve
- [ ] Step 5: Recommend downstream → user choose → invoke or stop
```

### Step 0 — Receive Rough Idea

Accept the user's input as inline text, a file path, or a GitHub issue URL.
- Issue URL: `gh issue view <url> --json title,body,labels`
- File path: `Read`
- Inline text: use as-is

Summarise in one sentence what you understood and confirm with the user before proceeding.

### Step 1 — Light Exploration (inline)

Using Grep / Glob / Read, identify 3–5 files or directories likely relevant to the idea. Hold the findings in working memory — they are conversation material, not part of the output.

**Escalation to Explore subagent:** if any of the following hold, pause and ask the user whether to dispatch an Explore subagent for a heavier survey:
- Candidate files span 10+ distinct paths
- Relevant code spans 3+ packages / top-level directories
- Unfamiliar frameworks or libraries dominate the area
- The user explicitly asks for a deeper survey

On escalation approval, dispatch Agent with `subagent_type: "Explore"` and `thoroughness: "medium"`. Fold the returned summary into the same working memory.

### Step 2 — Ground-Truth Check

Present what you found back to the user in 1–2 questions. Typical form:

> "This looks like it's around `path/foo.ts` and `path/bar.ts`. Correct, or are you thinking of a different area?"

Adjust and re-explore if the user corrects you.

### Step 3 — Shaping Dialogue

Ask questions **one at a time**. Prefer multiple-choice with 2–4 options + a recommendation with rationale. Cover, in order:

1. **Goal** — what we want to achieve and why (1–2 sentences).
2. **Constraints** — what not to do, rules to follow.
3. **Definition of Done** — verifiable completion criteria.

For each of the optional sections (`Approach Options`, `Out of Scope`, `Risks / Unknowns`, `Open Questions`), judge whether the task needs it. Ask **before** adding — default is YAGNI. Rule of thumb:
   - `Approach Options`: include when downstream planning still needs to choose an approach.
- `Out of Scope`: include when the idea is adjacent to other in-flight work.
- `Risks / Unknowns`: include when any Step 1 finding was surprising or unknown.
- `Open Questions`: include when there are real questions you are intentionally leaving for downstream.

### Step 4 — Write Spec and Get Approval

1. Read `templates/spec.md`.
2. Derive a kebab-case `{slug}` (~50 chars max) from the user's idea.
3. Fill it in:
   - `## Goal`, `## Constraints`, `## Definition of Done` — from Step 3 dialogue.
   - `## Context` — from Step 1 exploration and Step 2 confirmation (concrete file/module/path list).
   - Optional sections — only those the user opted into in Step 3.
   - Delete any optional section that was not opted into.
4. Determine `{base_dir}` per `## Output Location`, then write to `{base_dir}/specs/{timestamp}_{slug}.md`.
5. Show the path and ask: *"Please check this — edits or OK?"*
6. On edit requests, modify in place and re-confirm.

### Step 5 — Downstream Routing

Propose a downstream skill, then let the user choose:

| Suggest | When |
|---|---|
| **devflow** | Goal + DoD are sharp; work is a straight extension of existing code; approach has clear precedent. |
| **superpowers:writing-plans** | New design elements; the shape of the solution still needs to be decided. |
| **Stop here** | User prefers to take the spec elsewhere manually. |

Display in this form:

> "Recommended downstream: **{skill}** — {one-line reason}. Alternatives: {others}. Or stop here. Which?"

On choice, invoke the chosen skill via the `Skill` tool with the spec's absolute path as `args` (single string). Do not paste spec contents into `args` — the skill reads the file.

If the user stops here, end the session with the spec path and the suggested skill so they can invoke it later.

## Dialogue Principles

- **One question at a time** — never stack multiple open points in a single message.
- **Multiple choice preferred** — 2–4 options with your recommendation and a one-line reason each.
- **Ground-truth before shaping** — confirm which part of the codebase we are talking about (Step 2) before asking about Goal / Constraints / DoD.
- **YAGNI on sections** — keep to the required 4 unless there is a concrete reason to add an optional one.
- **Do not choose the approach here** — shaping-spec collects options; the downstream skill picks. The only exception is when the user explicitly decides during dialogue; in that case, record it in `## Approach Options` as "chosen: {option}".

## Rules

- **No code edits** — this skill only writes the spec file. It never modifies source code, tests, or build configuration.
- **No PLAN.md output** — if the conversation naturally produces step-by-step plans, trim them back to Goal / DoD / Approach Options and let the downstream skill expand.
- **Exploration is read-only** — Grep / Glob / Read only. Any write outside the spec file is a bug.
- **Downstream is a single hop** — invoke at most one downstream skill per session. If the user wants multiple, close this session and let them chain manually.
- **`## Downstream` is mandatory** — the spec must always end with a `## Downstream` section recording the recommended skill and a one-line reason. If the user opted for "Stop here", record that verbatim with the recommended skill still listed. Do not delete this section when pruning optional ones.
- **Spec is the single artifact** — do not also write WORKLOG / notes / scratch files. Everything that matters is in the spec file.
