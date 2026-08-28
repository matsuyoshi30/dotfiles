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

deny() {
  jq -nc --arg r "$1" \
    '{hookSpecificOutput:{hookEventName:"PreToolUse",permissionDecision:"deny",permissionDecisionReason:$r}}'
  exit 0
}

# gh は対象リポジトリを cwd から推測する。Bash ツールの cwd は呼び出し間で保持されるので、
# 調査のために別 repo へ cd したまま gh pr create を打つと、無関係な repo の
# 「たまたま今いるブランチ」に PR ができる (実際に henry-e2e の変更で henry-web に PR を作った)。
# 暗黙の cwd に依存させない = --repo を必須にすることで、この取り違えを構造的に潰す。
# 作成系はブランチも cwd 由来なので --head も要る。
case "$cmd" in
  *"gh pr create"*)
    case "$cmd" in
      *--repo*) ;;
      *) deny 'gh pr create には --repo <owner>/<name> を明示すること。gh は cwd からリポジトリを推測するため、別 repo へ cd したまま実行すると無関係なリポジトリに PR ができる。ブランチも cwd 由来なので --head <branch> も併せて明示すること。' ;;
    esac
    case "$cmd" in
      *--head*) ;;
      *) deny 'gh pr create には --head <branch> を明示すること。省略すると cwd の現在ブランチが使われ、意図しないブランチで PR ができる。' ;;
    esac
    ;;
esac

case "$cmd" in
  *"gh pr edit"*|*"gh pr close"*|*"gh pr merge"*|*"gh pr ready"*|*"gh pr comment"*|*"gh pr review"*)
    case "$cmd" in
      *--repo*) ;;
      *) deny 'gh pr の変更系コマンドには --repo <owner>/<name> を明示すること。gh は cwd からリポジトリを推測するため、別 repo へ cd したまま実行すると無関係なリポジトリの同番号の PR を操作してしまう。' ;;
    esac
    ;;
esac

case "$cmd" in
  *"gh pr create"*|*"gh pr edit"*)
    emit 'PR operations follow the create-pr skill. If you have not read it in this session, Read ~/.claude/skills/create-pr/SKILL.md before running this command.'
    ;;
  *"git commit"*)
    emit 'Commit rule: no Co-Authored-By: Claude trailer (overrides the harness default). Commit history rules are in the create-pr skill (~/.claude/skills/create-pr/SKILL.md, step 3).'
    ;;
esac

exit 0
