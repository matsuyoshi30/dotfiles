#!/usr/bin/env bash
# validate_artifact.sh <phase_id> <artifact_file>
# Structural validation: checks that Driver output contains required sections
# before passing to the Critic. Catches obvious omissions early.
set -euo pipefail
PHASE=$1; FILE=$2

if [ ! -f "$FILE" ]; then
  echo "ERROR: Artifact file not found: $FILE" >&2; exit 1
fi

errors=0
check_section() {
  local label=$1; local pattern=$2
  if ! grep -qiE "$pattern" "$FILE"; then
    echo "MISSING: $label (expected pattern: $pattern)" >&2
    errors=$((errors + 1))
  fi
}

case "$PHASE" in
  01)
    check_section "Background and Goal"      "^#+.*background|^#+.*goal"
    check_section "Scope"                     "^#+.*scope"
    check_section "Functional Requirements"   "^#+.*functional.?req"
    check_section "Non-Functional Requirements" "^#+.*non.?functional"
    check_section "Acceptance Criteria"       "^#+.*acceptance.?crit"
    check_section "Open Questions"            "^#+.*open.?question"
    ;;
  02)
    check_section "Architecture Overview"     "^#+.*architecture"
    check_section "Data Model"                "^#+.*data.?model"
    check_section "Technology Choices"        "^#+.*technology"
    check_section "Failure Modes"             "^#+.*failure"
    check_section "Test Strategy"             "^#+.*test.?strat"
    ;;
  03)
    check_section "Task definition (T01)"     "T01"
    check_section "depends_on field"          "depends_on"
    check_section "done_criteria field"       "done_criteria"
    check_section "test_strategy field"       "test_strategy"
    ;;
  04)
    # Implementation: just verify the notes file is non-empty
    if [ ! -s "$FILE" ]; then
      echo "MISSING: Implementation notes file is empty" >&2
      errors=$((errors + 1))
    fi
    ;;
  *)
    echo "WARNING: No structural validation defined for phase $PHASE" >&2
    ;;
esac

if [ "$errors" -gt 0 ]; then
  echo "Structural validation failed: $errors missing section(s)" >&2
  exit 1
fi
echo "[validate] Phase $PHASE artifact OK"
