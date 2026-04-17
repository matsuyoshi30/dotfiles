---
name: memory-manager
description: Manages work context in a 3-tier structure (short/mid/long-term), enabling memory restoration after compaction or session switches. Auto-fires via hooks at session start (restore short-term), on first user prompt (selectively load long-term/mid-term based on context), and before compaction (persist state). Also used when the user mentions memory, context restoration, or task resumption.
user-invocable: false
allowed-tools: Read, Write, Grep, Glob, Bash
---

# Memory Manager

Manage work context in a 3-tier structure, maintaining memory that survives compaction and session switches.

## Base Directory Resolution

Search in the following priority order and use the first one found as `{base}`. If neither exists, create `~/.matsuyoshi30/`.

1. `~/.matsuyoshi30/`
2. `~/.matsuyoshi/`
3. Create `~/.matsuyoshi30/`

Memory data is stored under `{base}/memory/`.

## 3-Tier Memory Structure

| Tier | Path | Content | Lifespan |
|------|------|---------|----------|
| Short-term | `{base}/memory/short-term/current.md` | Current work state and decision snapshots | During session. Deleted on task completion |
| Mid-term | `{base}/memory/mid-term/{task-id}/state.md` | In-progress task state and intermediate artifacts | Until task completion |
| Long-term | `{base}/memory/long-term/{topic}.md` | Project knowledge, learned conventions | Permanent |
| Index | `{base}/memory/index.md` | List of long-term memories (topic + one-line summary) | Permanent |

## Triggers and Actions

### On Session Start

1. Resolve the base directory
2. If `{base}/memory/short-term/current.md` exists, read it to restore previous work context
3. If `{base}/memory/index.md` exists, read it to identify relevant long-term memories
4. Read any relevant `{base}/memory/long-term/{topic}.md` files
5. Infer from current branch name or CWD and read any relevant `{base}/memory/mid-term/{task-id}/state.md`

### On Step Completion

Save short-term memory at meaningful checkpoints (investigation complete, implementation milestone, decision made).
Do this not every turn, but when information has accumulated that would be painful to lose to compaction.

Overwrite `{base}/memory/short-term/current.md` in the following format:

```markdown
# Current Session
## Date: {YYYY-MM-DD}
## CWD: {working directory}
## Branch: {branch name}

## Current Work
{what you are doing}

## Completed Steps
- {step}: {result}

## Next Steps
{what to do next}

## Decisions & Context
- {decision}: {rationale}
```

### On Task Boundary (commit, PR creation, branch switch)

Write to `{base}/memory/mid-term/{task-id}/state.md` in the following format.
Use branch name, ticket ID, or similar unique identifier as `task-id`.

```markdown
# Task: {task-id}
## Date: {YYYY-MM-DD}

## Overview
{task overview}

## Current Status
{how far along}

## Artifacts
- Branch: {branch name}
- Files: {changed files}

## Remaining Work
- {remaining items}

## Resumption Context
{background needed to resume later}
```

### On Task Completion

1. Clear short-term memory (delete `short-term/current.md`)
2. Delete the corresponding mid-term memory
3. If there are insights worth preserving long-term, write to `{base}/memory/long-term/{topic}.md`

Long-term memory format:

```markdown
# {topic name}
## Date: {YYYY-MM-DD}
## Source: {which task this was learned from}

{specific insight, convention, or pattern}
```

After writing, add an entry to `{base}/memory/index.md`:

```markdown
- [{topic}](long-term/{topic}.md) — {one-line summary}
```

## Guidelines

- Don't write short-term memory too frequently — do it at meaningful step boundaries
- Mid-term memory must include artifact paths, URLs, and other info needed for resumption
- Long-term memory is for general insights only. Don't include task-specific details
- MEMORY.md manages user info, feedback, and references. This skill manages session work state, task progress, and technical insights. Use both together
