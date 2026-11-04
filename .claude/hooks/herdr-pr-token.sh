#!/bin/bash
# SessionStart / Stop: herdr のサイドバーに出す $pr トークンを、このセッションが作った open な PR の番号で更新する。
# PR は起動ディレクトリとは別のリポジトリやワークツリーで作ることが多いので、cwd のブランチではなく
# Claude Code がトランスクリプトに残す pr-link から拾う。PR はセッションの途中で作られるので、ターンごとの Stop で追従する。
set -uo pipefail

[ "${HERDR_ENV:-}" = "1" ] && [ -n "${HERDR_PANE_ID:-}" ] || exit 0

transcript=$(jq -r '.transcript_path // empty' 2>/dev/null) || exit 0
[ -n "$transcript" ] && [ -f "$transcript" ] || exit 0

# gh の API 呼び出しを待つとターンの終わりが遅れるので、切り離して返る。
(
  prs=()
  while IFS=$'\t' read -r repo number; do
    state=$(gh pr view "$number" --repo "$repo" --json state --jq .state 2>/dev/null)
    [ "$state" = OPEN ] && prs+=("#$number")
  done < <(grep -F '"type":"pr-link"' "$transcript" |
    jq -r '[.prRepository, .prNumber] | @tsv' 2>/dev/null | awk '!seen[$0]++')

  if [ "${#prs[@]}" -gt 0 ]; then
    herdr pane report-metadata "$HERDR_PANE_ID" --source dotfiles:pr --token "pr=${prs[*]}"
  else
    herdr pane report-metadata "$HERDR_PANE_ID" --source dotfiles:pr --clear-token pr
  fi
) </dev/null >/dev/null 2>&1 &
