#!/bin/bash
# Short-term memory is keyed by session_id so that parallel sessions never restore
# or overwrite each other's state. Hook input is the only place the id is known,
# so this script hands the per-session path to the model.
set -uo pipefail

input=$(cat)
event=$(jq -r '.hook_event_name // ""' <<<"$input" 2>/dev/null) || exit 0
session_id=$(jq -r '.session_id // ""' <<<"$input" 2>/dev/null) || exit 0
[[ -n "$event" && -n "$session_id" ]] || exit 0

memory="$HOME/.matsuyoshi30/memory"
short_term="$memory/short-term/$session_id.md"
# Settings-file hooks ignore `once`, so first-prompt-only is enforced with a marker
# that SessionStart clears, which also re-arms the index restore after compaction.
prompted="${TMPDIR:-/tmp}/claude-memory-context/$session_id.prompted"
isolation="This session's short-term file is $short_term. Other files in $memory/short-term/ belong to sessions that may be running in parallel: never read or write them. Mid-term state.md and index.md can be shared with parallel sessions, so re-read them right before writing and change only your own task's part with Edit instead of rewriting the whole file."

case "$event" in
  SessionStart)
    rm -f "$prompted"
    find "$memory/short-term" -maxdepth 1 -type f \
      -name '????????-????-????-????-????????????.md' ! -name "$session_id.md" \
      -mtime +14 -delete 2>/dev/null
    if [[ -f "$short_term" ]]; then
      context="Auto-fire memory-manager skill (restore phase): Read $short_term to restore this session's state. Do NOT read index.md, long-term, or mid-term files yet — wait for the user first prompt to filter by relevance. $isolation"
    else
      context="memory-manager: $isolation"
    fi
    ;;
  UserPromptSubmit)
    [[ -f "$prompted" ]] && exit 0
    mkdir -p "$(dirname "$prompted")" && : >"$prompted"
    context="Auto-fire memory-manager skill (context-aware restore, first prompt only): Read $memory/index.md. Based on the user prompt, current branch name, and CWD, selectively read only the relevant $memory/long-term/*.md files and the matching $memory/mid-term/{task-id}/state.md if one exists. Skip entries that are not relevant to the current task. No hook can prompt a save before compaction, so save state to $short_term at meaningful checkpoints per the memory-manager short-term template. $isolation"
    ;;
  *)
    exit 0
    ;;
esac

jq -nc --arg event "$event" --arg context "$context" \
  '{hookSpecificOutput: {hookEventName: $event, additionalContext: $context}}'
