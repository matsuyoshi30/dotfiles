---
name: memory
description: Manual command for checking memory status, saving, searching, and cleaning up
user-invocable: true
allowed-tools: Read, Write, Grep, Glob, Bash
---

# /memory Command

Check memory status, manually save, search, and clean up memories.

## Base Directory Resolution

Search in the following priority order and use the first one found as `{base}`. If neither exists, create `~/.matsuyoshi30/`.

1. `~/.matsuyoshi30/`
2. `~/.matsuyoshi/`
3. Create `~/.matsuyoshi30/`

Memory data is stored under `{base}/memory/`.

## Subcommands

Parse the arguments and execute the appropriate subcommand.

### `/memory` (no arguments)

Display a summary of the current memory state.

1. Check if `{base}/memory/short-term/current.md` exists; if so, display a summary of its content
2. List directories under `{base}/memory/mid-term/` (each task-id with last modified date)
3. Display file count under `{base}/memory/long-term/` and the content of `index.md`

Output format:

```
## Memory Status

### Short-term
{summary of current.md, or "None"}

### Mid-term ({N} tasks)
- {task-id}: {last modified date}
- ...

### Long-term ({N} entries)
{content of index.md}
```

### `/memory save`

Immediately save the current work state to short-term memory.

1. Organize the current session's work content, completed steps, next steps, and decisions
2. Write to `{base}/memory/short-term/current.md` (following the short-term template from memory-manager skill)
3. Report completion

### `/memory search {keyword}`

Search long-term memories by keyword.

1. Read `{base}/memory/index.md` and display entries matching the keyword
2. Grep files under `{base}/memory/long-term/` for the keyword
3. Display the content of matching memories

### `/memory clean`

Clean up unnecessary memories.

1. If `{base}/memory/short-term/current.md` exists and its date is stale (before today), ask whether to delete
2. List tasks under `{base}/memory/mid-term/` whose associated branches have been deleted
3. Confirm deletion targets with the user before deleting (never delete without confirmation)
