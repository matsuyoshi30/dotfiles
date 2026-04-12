# Scripts

## CLI Flag Compatibility

`run_phase.sh` maps CLI-specific flags automatically:

| CLI | System prompt flag | Notes |
|---|---|---|
| `claude` | `--system-prompt-file` | Also uses `--input`, `--instruction` |
| `codex` | `--system` | Also uses `--input`, `--instruction` |
| `cursor-agent` | `--system` | Also uses `--input`, `--instruction` |

If a CLI updates its flag syntax, update the `sys_flag_for()` function in `run_phase.sh`.

## Model IDs

Concrete model IDs are defined in `run.sh` — update them there when models change.

## Design Principles

1. **Driver ≠ Critic is enforced.** Same-model self-review shares blind spots, so it is prohibited.
2. **The Critic only critiques, never rewrites.** Revisions always go back to the Driver.
3. **Critic output is structured JSON.** Natural language reviews let the Driver dismiss feedback ambiguously.
4. **Only artifact files are passed between phases.** Conversation history is not carried over (saves context window + forces decisions to be documented).
5. **Roles swap between phases.** Extends Anthropic's Advisor Strategy by alternating Driver/Critic roles for cross-model review.

## Structural Validation

Before the Critic reviews, `validate_artifact.sh` checks that the Driver output contains all required sections for the phase (e.g., "Acceptance Criteria" in requirements, "T01" + "done_criteria" in implementation plan). This catches obvious omissions without spending a Critic round trip.
