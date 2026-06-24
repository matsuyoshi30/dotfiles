---
name: session-handoff
description: Create a handoff so work can be continued in a new session. Use when the user wants to hand off to another session, wants a handoff prompt, or wants to start a new session and continue.
user-invocable: true
allowed-tools: Read, Write, Grep, Glob, Bash
---

# Session Handoff

From the current session state, generate a handoff md that a new session can read standalone to resume work,
and present a short start prompt to paste into the new session.

The handoff md is created as a self-contained snapshot + pointers that survives even if short-term/current.md is overwritten by another task.
Because memory-manager is user-invocable: false (hook-driven) and cannot be called directly, Write it yourself following the format defined in memory-manager SKILL.md (do not manage it in two places).

## Base Directory Resolution

Use the first one found, in the following priority order, as `{base}`. If neither exists, create `~/.matsuyoshi30/`.

1. `~/.matsuyoshi30/`
2. `~/.matsuyoshi/`

## Workflow

1. Resolve `{base}`.
2. Because memory-manager is user-invocable: false (hook-driven) and cannot be called directly, Write it yourself following the format defined in memory-manager SKILL.md.
   - If the `{base}/memory/short-term/` directory does not exist, create it.
   - Always update `{base}/memory/short-term/current.md` every time session-handoff runs (regardless of whether it is a task boundary). Follow the memory-manager Current Session format.
   - If a `task-id` (branch name / ticket ID, etc.) can be identified, create the `{base}/memory/mid-term/{task-id}/` directory if it does not exist, and update `{base}/memory/mid-term/{task-id}/state.md`. Follow the memory-manager Task format. Write it even if it is not a formal task boundary (commit / PR / branch switch), as long as a task-id can be identified. If no task-id can be identified, skip writing to mid-term and rely on the self-contained handoff md.
3. Extract and consolidate the information needed for the handoff from the conversation. Rather than copying memory verbatim,
   articulate "the specific first move the new session should take" and "decisions / gotchas not yet written to memory".
4. If the `{base}/handoff/` directory does not exist, create it, and write out `{base}/handoff/YYYY-MM-DD-{topic}.md` using the template below.
   - Derive `{topic}` from the task-id / branch name. If no task-id can be identified, confirm a short label with the user. If no answer is obtained, use a date-based sequence like `YYYY-MM-DD-1`, `YYYY-MM-DD-2`.
   - If a file with the same name exists, append a sequence number like `-2`, `-3`.
5. Output the following two items to the user.
   - The absolute path of the generated handoff md.
   - A one-line start prompt to paste into the new session (below).

## Handoff md template

Do not use horizontal rules or bold. Structure it with headings and plain text.

```markdown
# Handoff: {task-id / topic}
## Date: {YYYY-MM-DD}
## Repo / Branch / CWD: {repo} / {branch} / {cwd}

## Task summary
{1-2 lines}

## What's done
- {step}: {result}

## Next steps (first move)
{the concrete action the new session should take first}

## Key decisions / assumptions / gotchas
- {decision/gotcha}: {reason}

## What to reference
- mid-term state: {path, omit if none}
- plan / spec: {path, omit if none}
- key code: {file:line, omit if none}
```

## Start prompt to hand to the new session

Finally, present the following one line as-is.

> Continuing from the previous session. Read `{handoff path}` and continue from "Next steps".

In the new session, memory-manager's auto-restore also kicks in, but the handoff md functions as a self-contained snapshot.

## Guidelines

- Prioritize the handoff md's self-containedness so work can resume standalone. Supplement deep context with references to mid-term state / plan.
- Do not delete or manage the lifecycle of the handoff md. Leave cleanup at task completion to memory-manager.
- Do not auto-launch the new session. The premise is that the user manually starts the new session.
