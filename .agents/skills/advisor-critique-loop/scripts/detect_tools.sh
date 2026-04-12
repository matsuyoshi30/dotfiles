#!/usr/bin/env bash
# Detect available CLI agents and write .advisor/tools.json
set -euo pipefail
mkdir -p .advisor
has() { command -v "$1" >/dev/null 2>&1 && echo true || echo false; }
cat > .advisor/tools.json <<EOF
{
  "claude": $(has claude),
  "codex":  $(has codex),
  "cursor": $(has cursor-agent),
  "implementer": "$(command -v cursor-agent >/dev/null 2>&1 && echo cursor-composer-2-fast || echo claude-sonnet-4-6)"
}
EOF
cat .advisor/tools.json
# Hard requirements
for t in claude codex jq; do
  command -v "$t" >/dev/null 2>&1 || { echo "ERROR: '$t' is required but not found on PATH" >&2; exit 1; }
done
