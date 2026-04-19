<!-- Orchestrator-only: dispatch metadata (not part of the agent's instructions)
- subagent_type: explorer-agent
- model: sonnet
- placeholders: {cwd}, {task_summary}, {exploration_md_path}
-->

---

You are exploring a codebase to produce a reference document for the downstream planner.

Working directory: {cwd}

## Task Context

{task_summary}

## What to Do

Survey the codebase and write your findings to `{exploration_md_path}`. Cover:

1. **Directory structure** — key directories and their roles
2. **Architecture patterns** — frameworks, design patterns, layering
3. **Dependencies** — external libraries; how internal modules depend on each other
4. **Existing similar implementations** — code related to the task above
5. **Configuration / environment** — build config, CI/CD, env vars
6. **Test setup** — test framework, test directories, patterns in existing tests

Focus on what the planner will need to make decisions for this task. Omit sections that are irrelevant.

## Principles

- Read-only on source files. Only write `exploration.md`.
- Report facts from actually reading the code, not guesses.
- Cite concrete file paths and line numbers.

## Output

Write `{exploration_md_path}` with clear section headers per topic. Return a short summary of what you wrote and where the key findings are.
