# Evaluation Guide

## Running evals

Each JSON file defines a scenario with `skills`, `query`, `expected_behavior`, and optional `preconditions`.

### Preconditions

When a scenario has `preconditions` (e.g., `{"cursor-agent": "not installed"}`), ensure the specified tool is **not on PATH** before running. For example, temporarily rename/remove the binary or run in a restricted `PATH` environment:

```bash
PATH=$(echo "$PATH" | tr ':' '\n' | grep -v cursor | tr '\n' ':') bash scripts/run.sh request.md
```

## Model-specific testing

This skill orchestrates multiple models (Opus, Codex, Sonnet). Test each eval under these configurations:

| Config | Driver models | Critic models | Notes |
|---|---|---|---|
| Default | Opus + Codex + Sonnet | Codex + Opus | Full pipeline as designed |
| Sonnet-only fallback | Replace Opus with Sonnet | Codex | Verify quality when Opus is unavailable |
| High-latency | Default | Default | Add artificial delay (`sleep 2` in run_phase.sh) to simulate slow API responses; verify timeout/escalation behavior |

### What to watch for per model

- **Opus as Driver**: May over-engineer requirements/plans. Check that YAGNI rule is respected.
- **Sonnet as Driver (fallback)**: May produce less detailed artifacts. Verify acceptance criteria are still testable.
- **Codex as Critic**: May return overly verbose issues. Verify JSON output stays within schema.
- **Codex as Driver (Phase 2)**: May prefer different technology stacks. Verify rationale is documented.
