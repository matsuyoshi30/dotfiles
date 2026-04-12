#!/usr/bin/env bash
# run.sh — entrypoint for advisor-critique-loop
# Usage: ./run.sh <user_request_file>
set -euo pipefail
REQ=${1:?usage: run.sh <user_request.md>}
ROOT=$(cd "$(dirname "$0")/.." && pwd)
RUN=$(date +%Y%m%d-%H%M%S)
OUT=".advisor/$RUN"
mkdir -p "$OUT"

bash "$ROOT/scripts/detect_tools.sh"
IMPL=$(jq -r '.implementer' .advisor/tools.json)

OPUS="claude --model claude-opus-4-6"
CODEX="codex --model gpt-5.4-high"
if [ "$IMPL" = "cursor-composer-2-fast" ]; then
  IMPL_CMD="cursor-agent --model composer-2-fast"
else
  IMPL_CMD="claude --model claude-sonnet-4-6"
fi

P="$ROOT/prompts"
CRITIC="$P/_critic_template.md"
RP="$ROOT/scripts/run_phase.sh"

# Phase 1: Requirements — Opus drives, Codex critiques
"$RP" 01 "$OPUS"  "$CODEX" "$P/01_req_driver.md"    "$CRITIC" "$REQ"                "$OUT/01_requirements.md"

# Phase 2: Design — Codex drives, Opus critiques
"$RP" 02 "$CODEX" "$OPUS"  "$P/02_design_driver.md" "$CRITIC" "$OUT/01_requirements.md" "$OUT/02_design.md"

# Phase 3: Implementation plan — Opus drives, Codex critiques
"$RP" 03 "$OPUS"  "$CODEX" "$P/03_plan_driver.md"   "$CRITIC" "$OUT/02_design.md"       "$OUT/03_impl_plan.md"

# Phase 4: Implementation — Implementer drives, Codex critiques (per-task loop handled inside)
"$RP" 04 "$IMPL_CMD" "$CODEX" "$P/04_impl_driver.md" "$CRITIC" "$OUT/03_impl_plan.md"   "$OUT/04_impl_notes.md"

echo "=== Pipeline complete. Artifacts in $OUT ==="
ls -la "$OUT"
