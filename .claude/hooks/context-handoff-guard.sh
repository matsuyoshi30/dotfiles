#!/bin/bash
# Stop: nudge for a session handoff before the context window fills up, so work can
# move to a fresh session instead of being compacted.
#
# Hook inputs carry no context-usage figures at all, and PreCompact is too late —
# that event *is* the compaction. The statusline is the only place Claude Code hands
# out the real numbers, so it relays them through ${TMPDIR}/claude-ctx/{session_id}.json
# (see .claude/statusline.py). No statusline (headless `claude -p`) means no file,
# and this hook stays silent.
set -uo pipefail

input=$(cat)
session_id=$(printf '%s' "$input" | jq -r '.session_id // ""' 2>/dev/null) || exit 0
[[ -n "$session_id" ]] || exit 0

state_dir="${TMPDIR:-/tmp}/claude-ctx"
state="$state_dir/$session_id.json"
fired="$state_dir/$session_id.fired"

[[ -f "$state" ]] || exit 0

pct=$(jq -r '.pct // empty' "$state" 2>/dev/null) || exit 0
[[ "$pct" =~ ^[0-9]+$ ]] || exit 0

# 85% is measured, not derived. Across 41 transcripts, auto-compaction never fired
# once — the three compactions on record were all manual `/compact` at 1.02M-1.09M
# tokens on a 1M window, and plenty of sessions ran to 900k untouched. So
# CLAUDE_AUTOCOMPACT_PCT_OVERRIDE is not the baseline to subtract from; real
# pressure starts near the top of the window. 85% leaves ~150k of headroom, which
# is far more than a handoff md needs.
threshold="${CLAUDE_HANDOFF_PCT:-85}"
[[ "$threshold" =~ ^[0-9]+$ ]] || exit 0

# Rising edge only: go quiet once fired, and re-arm when a compaction drops usage
# back below the threshold. The marker file is the whole state machine.
if (( pct < threshold )); then
  rm -f "$fired"
  exit 0
fi
[[ -f "$fired" ]] && exit 0

# Marker before emit, so the Stop that follows this one is already silent — that,
# not restraint, is what keeps `decision: block` from looping.
#
# It has to be `block`. Stop's additionalContext was measured to be inert: a probe
# hook emitting it fired (marker written) yet the model answered as if nothing had
# been injected, while the same probe emitting `block` was obeyed verbatim.
: > "$fired"

jq -nc --arg reason "Context window is at ${pct}%, past the ${threshold}% handoff mark. Run the session-handoff skill now: write the handoff md, then show the user its path and the one-line start prompt for a new session. Do not start new work after that — wait for the user to choose between continuing here and moving to a fresh session. This fires once; it re-arms only if usage drops back below ${threshold}%." \
  '{decision:"block",reason:$reason}'

exit 0
