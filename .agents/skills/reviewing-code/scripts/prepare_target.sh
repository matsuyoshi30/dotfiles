#!/usr/bin/env bash
# Writes intent.md, diff.txt (line-annotated), and hunks.json for a review target into <out_dir>.
#
#   prepare_target.sh <out_dir> pr <PR number or URL>
#   prepare_target.sh <out_dir> local [--base <rev>] [path...]
#
# local: the diff from <rev> (default: merge-base of HEAD and origin/HEAD) to the working tree,
# including untracked files. A named path with no change is included whole, as an added file.
set -euo pipefail

usage() { sed -n 4,5p "$0" | sed 's/^# *//' >&2; exit 2; }
[ $# -ge 2 ] || usage
out=$1 mode=$2
shift 2
mkdir -p "$out"
parse_diff="$(cd "$(dirname "$0")" && pwd)/parse_diff.py"

owner_repo() { sed -E 's#^(https?://|ssh://)?([^@/]+@)?[^/:]+[/:]##; s#\.git$##; s#^(([^/]+)/([^/]+)).*#\1#'; }

case $mode in
pr)
  [ $# -eq 1 ] || usage
  pr=$1
  pr_url=$(gh pr view "$pr" --json url -q .url)
  pr_repo=$(owner_repo <<<"$pr_url")
  origin_repo=$(git remote get-url origin | owner_repo)
  if [ "$(tr '[:upper:]' '[:lower:]' <<<"$pr_repo")" != "$(tr '[:upper:]' '[:lower:]' <<<"$origin_repo")" ]; then
    echo "repository mismatch: the PR belongs to $pr_repo, but origin of $(pwd) is $origin_repo" >&2
    exit 3
  fi
  gh pr view "$pr" >"$out/intent.md"
  gh pr diff "$pr" | python3 "$parse_diff" "$out/hunks.json" >"$out/diff.txt"
  ;;
local)
  base=
  if [ "${1:-}" = --base ]; then
    [ $# -ge 2 ] || usage
    base=$2
    shift 2
  fi
  if [ -z "$base" ]; then
    if ! base=$(git merge-base HEAD origin/HEAD 2>/dev/null); then
      base=$(git rev-parse HEAD)
      echo "warning: origin/HEAD is not set, so only uncommitted changes are reviewed; pass --base <rev> to include commits" >&2
    fi
  fi
  {
    if pr_body=$(gh pr view 2>/dev/null); then
      printf '%s\n\n' "$pr_body"
    fi
    git log --format='%s%n%n%b' "$base..HEAD"
  } >"$out/intent.md"
  {
    git diff --no-ext-diff --src-prefix=a/ --dst-prefix=b/ "$base" -- "$@"
    {
      git ls-files -z --others --exclude-standard -- "$@"
      for p in "$@"; do
        if [ -z "$(git diff --name-only "$base" -- "$p")$(git ls-files --others --exclude-standard -- "$p")" ]; then
          git ls-files -z -- "$p"
        fi
      done
    } | while IFS= read -r -d '' f; do
      git diff --no-ext-diff --no-index --src-prefix=a/ --dst-prefix=b/ -- /dev/null "$f" || [ $? -eq 1 ]
    done
  } | python3 "$parse_diff" "$out/hunks.json" >"$out/diff.txt"
  ;;
*)
  usage
  ;;
esac
