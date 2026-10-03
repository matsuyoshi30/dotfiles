---
name: memory-manager
description: Restore and persist short-, mid-, and long-term work context when invoked by memory lifecycle hooks.
allowed-tools: Read, Write, Grep, Glob, Bash
user-invocable: false
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
| Short-term | `{base}/memory/short-term/{session_id}.md` | Current work state and decision snapshots | During session. Session files untouched for 14 days are pruned by the SessionStart hook |
| Mid-term | `{base}/memory/mid-term/{task-id}/state.md` | In-progress task state and intermediate artifacts | Until task completion |
| Long-term | `{base}/memory/long-term/{topic}.md` | Project knowledge, learned conventions | Permanent |
| Index | `{base}/memory/index.md` | One line per long-term memory and per in-progress mid-term task | Permanent |

## Parallel Sessions

Several sessions often run at once, so the hooks (`.claude/hooks/memory-context.sh`) pass this session's short-term path, derived from `session_id`. Use only that path. Other files in `short-term/` belong to other sessions; never read or write them. If no path has been given in this session, skip short-term reads and writes.

The path survives compaction and `--resume`, which keep the session_id. `/clear` and forks start a new session_id with an empty short-term file; to carry work across them, use session-handoff.

Mid-term `state.md` and `index.md` can be touched by parallel sessions working on the same task. Re-read them right before writing and change only your own task's part with Edit; never rewrite either file from memory.

## Triggers and Actions

### On Session Start

1. Resolve the base directory
2. If this session's short-term file exists, read it to restore previous work context
3. If `{base}/memory/index.md` exists, read it to identify relevant long-term memories
4. Read any relevant `{base}/memory/long-term/{topic}.md` files
5. Infer from current branch name or CWD and read any relevant `{base}/memory/mid-term/{task-id}/state.md`

### On Step Completion

Save short-term memory at meaningful checkpoints (investigation complete, implementation milestone, decision made).
Do this not every turn, but when information has accumulated that would be painful to lose to compaction. This is the only save before compaction: PreCompact hooks cannot inject instructions to the model, so nothing prompts a save at the moment compaction starts.

Overwrite this session's short-term file in the following format:

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

A task is complete when its PRs are merged or closed, which usually happens outside any session. Whenever you observe that, including while checking the status of an index entry's PRs:

1. Delete this session's short-term file if it covered that task
2. Delete the corresponding mid-term directory and its line in `index.md`
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

When a mid-term task is first written, add one line for it as well, naming its PRs so completion can be checked later:

```markdown
- [{task-id} state](mid-term/{task-id}/state.md) — {one-line summary}. PR {repo} #{n}
```

Every index entry is a single line of at most about 150 characters. The index is read at the start of every session, so details belong in the linked file, not the index.

## Guidelines

- Don't write short-term memory too frequently — do it at meaningful step boundaries
- Mid-term memory must include artifact paths, URLs, and other info needed for resumption
- Long-term memory is for general insights only. Don't include task-specific details
- MEMORY.md manages user info, feedback, and references. This skill manages session work state, task progress, and technical insights. Use both together
