Investigate the codebase to prepare context for an implementer who will work on the following task.

Working directory: {cwd}

## Task

{resolved_spec_text}

## Goal

Answer: "What does the implementer need to know before writing code?" Cover the project's overall structure, task-relevant code and its dependencies, existing patterns/conventions to follow, test approach, and impact areas.

Be specific — include file paths and line numbers, not vague descriptions. Find the closest precedent (a similar feature already implemented) if one exists.

## Report Format

```
## Project Overview
(2-3 sentence summary of the project structure and tech stack)

## Relevant Files
- path/to/file.ts:L10-L50 — role relative to the task

## Existing Patterns
- Pattern: description
  Example: path/to/file.ts:L20 — how it's done

## Test Approach
- Test location, framework, relevant existing tests

## Impact Areas
- path/to/dependent.ts — why it might be affected

## Recommended Approach
(How to approach the implementation, referencing patterns and precedents found above.)
```

Omit sections with no relevant findings.
