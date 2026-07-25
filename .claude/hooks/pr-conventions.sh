#!/bin/bash
# PreToolUse(Bash): inject PR and commit conventions right before they are needed.
# A skill cannot reach subagents that lack the Skill tool (dev-workflow's create-pr-agent
# pins skills: create-pr), so the rules are delivered at Bash execution instead —
# the one point every path goes through.
set -uo pipefail

input=$(cat)
cmd=$(printf '%s' "$input" | jq -r '.tool_input.command // ""' 2>/dev/null) || exit 0

emit() {
  jq -nc --arg ctx "$1" \
    '{hookSpecificOutput:{hookEventName:"PreToolUse",additionalContext:$ctx}}'
  exit 0
}

case "$cmd" in
  *"gh pr create"*|*"gh pr edit"*)
    emit 'PR operations follow the create-pr skill. If you have not read it in this session, Read ~/.claude/skills/create-pr/SKILL.md before running this command.'
    ;;
  *"git commit"*)
    emit 'Commit rule: no Co-Authored-By: Claude trailer (overrides the harness default). Commit history rules are in the create-pr skill (~/.claude/skills/create-pr/SKILL.md, step 2).'
    ;;
esac

exit 0
