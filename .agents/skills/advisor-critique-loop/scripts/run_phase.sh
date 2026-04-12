#!/usr/bin/env bash
# run_phase.sh <phase_id> <driver_cmd> <critic_cmd> <driver_prompt> <critic_prompt> <input_file> <output_file>
# Loops: driver -> critic -> (if NEEDS_FIX) driver-fix -> critic -> ...
set -euo pipefail
PHASE=$1; DRIVER=$2; CRITIC=$3; DP=$4; CP=$5; IN=$6; OUT=$7
# Empirically, blocker-level issues converge in 2-3 round trips.
# Beyond 4, unresolved issues typically indicate a model capability limit,
# so escalating to a human is more efficient than continuing.
MAX_ITER=${MAX_ITER:-4}
if [ "$MAX_ITER" -lt 1 ]; then
  echo "ERROR: MAX_ITER must be >= 1 (got $MAX_ITER)" >&2; exit 1
fi
VERDICT=""
RUN_DIR=$(dirname "$OUT")
mkdir -p "$RUN_DIR"

read -ra DRIVER_ARGS <<< "$DRIVER"
read -ra CRITIC_ARGS <<< "$CRITIC"

# Determine the correct system-prompt flag for each CLI.
#   Claude CLI uses --system-prompt-file; Codex and cursor-agent use --system.
sys_flag_for() {
  case "$1" in
    claude*) echo "--system-prompt-file" ;;
    *)       echo "--system" ;;
  esac
}
DRIVER_SYS_FLAG=$(sys_flag_for "${DRIVER_ARGS[0]}")
CRITIC_SYS_FLAG=$(sys_flag_for "${CRITIC_ARGS[0]}")

VALIDATE="$(dirname "$0")/validate_artifact.sh"

echo "[phase $PHASE] driver: $DRIVER"
"${DRIVER_ARGS[@]}" "$DRIVER_SYS_FLAG" "$DP" --input "$IN" > "$OUT"

# Structural validation: catch missing sections before Critic review
if [ -x "$VALIDATE" ]; then
  "$VALIDATE" "$PHASE" "$OUT"
fi

# Map phase ID to the section name the Critic template expects,
# so the Critic applies only the matching review focus section.
case "$PHASE" in
  01) PHASE_NAME="Requirements" ;;
  02) PHASE_NAME="Design" ;;
  03) PHASE_NAME="Implementation Plan" ;;
  04) PHASE_NAME="Implementation" ;;
  *)  PHASE_NAME="$PHASE" ;;
esac

prev_issues=""
for i in $(seq 1 "$MAX_ITER"); do
  REVIEW="$RUN_DIR/${PHASE}_review_$i.json"
  echo "[phase $PHASE] critic iter=$i"
  "${CRITIC_ARGS[@]}" "$CRITIC_SYS_FLAG" "$CP" --input "$OUT" \
    --instruction "Current phase: $PHASE_NAME. Apply only the '$PHASE_NAME' review focus section." > "$REVIEW"

  # Validate that the Critic returned valid JSON before parsing
  if ! jq empty "$REVIEW" 2>/dev/null; then
    echo "" >&2
    echo "=== ESCALATION: Phase $PHASE critic returned invalid JSON ===" >&2
    echo "Raw output saved to: $REVIEW" >&2
    echo "Action: verify the Critic CLI is working correctly and returning JSON." >&2
    exit 4
  fi

  VERDICT=$(jq -r '.verdict' "$REVIEW")
  if [ "$VERDICT" = "LGTM" ]; then
    echo "[phase $PHASE] accepted at iter=$i"; break
  fi

  # Detect stuck loop (same issues raised twice in a row)
  cur=$(jq -r '[.issues[].problem] | join("|")' "$REVIEW")
  if [ "$cur" = "$prev_issues" ]; then
    echo "" >&2
    echo "=== ESCALATION: Phase $PHASE stuck ===" >&2
    echo "Driver failed to resolve the same issues two iterations in a row." >&2
    echo "Unresolved issues:" >&2
    jq -r '.issues[] | "  [\(.severity)] \(.where): \(.problem)"' "$REVIEW" >&2
    echo "Review file: $REVIEW" >&2
    echo "Action: inspect the review, manually fix the artifact, or revise the requirements/design." >&2
    exit 2
  fi
  prev_issues="$cur"

  echo "[phase $PHASE] driver fix iter=$i"
  cat "$OUT" "$REVIEW" > "$RUN_DIR/${PHASE}_fix_input_$i.md"
  "${DRIVER_ARGS[@]}" "$DRIVER_SYS_FLAG" "$DP" \
    --input "$RUN_DIR/${PHASE}_fix_input_$i.md" \
    --instruction 'Resolve every blocker and major issue in the attached review JSON. Return the full updated artifact.' > "$OUT"
done

# If the loop ended via break (LGTM), exit normally
if [ "$VERDICT" = "LGTM" ]; then
  exit 0
fi

echo "" >&2
echo "=== ESCALATION: Phase $PHASE reached MAX_ITER ($MAX_ITER) ===" >&2
echo "Final review result:" >&2
LAST_REVIEW="$RUN_DIR/${PHASE}_review_${MAX_ITER}.json"
if [ -f "$LAST_REVIEW" ]; then
  jq -r '.issues[] | "  [\(.severity)] \(.where): \(.problem)"' "$LAST_REVIEW" >&2
  echo "Review file: $LAST_REVIEW" >&2
fi
echo "Action: inspect remaining issues, manually fix, or re-run with a higher --max-iter." >&2
exit 3
