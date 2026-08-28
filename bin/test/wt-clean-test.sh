#!/usr/bin/env bash
#
# wt-clean-test.sh
#
# 合成リポジトリを temp に作り、wt-clean の dry-run 分類を期待値と突き合わせる。
# 破壊的なツールなので、判定表そのものを回帰対象にする。
#
# gh 依存のケースは PATH 先頭に置いた fake gh で決定的にする。
# 実ネットワークには一切出ない。
#
set -uo pipefail

WTCLEAN="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/wt-clean"
[[ -x "$WTCLEAN" ]] || { echo "wt-clean が無い: $WTCLEAN" >&2; exit 2; }

PASS=0; FAIL=0
ok()   { PASS=$((PASS+1)); printf '  ok   %s\n' "$1"; }
ng()   { FAIL=$((FAIL+1)); printf '  FAIL %s\n' "$1"; }

# 候補一覧の行は「REPO TIER SIZE UNTRACKED BRANCH PATH」。
# tier 名で候補行を絞り、その中に識別子 (ブランチ名か worktree パスの一部) が
# 含まれるかで照合する。detached はブランチ列が "-" になるためパスで照合する。
assert_tier() {
  local out="$1" what="$2" tier="$3"
  if grep -E "[[:space:]]${tier}[[:space:]]" <<<"$out" | grep -qF "$what"; then
    ok "$what => $tier"
  else
    ng "$what => $tier (候補行: $(grep -F "$what" <<<"$out" | tr -s ' ' || echo none))"
  fi
}

# 「候補一覧に出ない」だけでは不十分: wt-clean が落ちて出力が空でも grep は
# 何もマッチせず「候補外」に見えてしまう (何も無いことは何の証拠にもならない)。
# dry-run が実際に完走したという積極的な証拠 (出力が非空 かつ 正常終了) が
# 無い限り「候補外」とは判定しない。rc は呼び出し側が run_wtclean 直後に
# $? を取って渡すこと (途中に他のコマンドを挟むと $? が上書きされる)。
assert_not_candidate() {
  local out="$1" rc="$2" what="$3"
  if [[ -z "$out" || "$rc" -ne 0 ]]; then
    ng "$what は候補外 (dry-run が完走していない: rc=$rc, 出力=$([[ -z "$out" ]] && echo 空 || echo 非空))"
    return
  fi
  if grep -E "[[:space:]](reachable|squash|merged-else|closed-pr|landed-pr)[[:space:]]" <<<"$out" \
     | grep -qF "$what"; then
    ng "$what は候補に出てはいけない"
  else
    ok "$what は候補外"
  fi
}

assert_reported() {
  local out="$1" pat="$2" label="$3"
  if grep -q "$pat" <<<"$out"; then ok "$label"; else ng "$label"; fi
}

# lsof が使えない状況を作り、detached が候補から外れる (fail closed) ことを見る。
# 「detached が出ない」だけでは候補を全部落としても真になってしまうので、
# branch 付きの reachable (old-merged) は引き続き候補に出ることも確認し、
# fail closed の範囲が detached だけに絞られていることを固定する。
assert_fail_closed() {
  local root="$1" stub="$T/failbin" out
  mkdir -p "$stub"; cp "$FAKE_GH_BIN/gh" "$stub/gh"
  printf '#!/usr/bin/env bash\nexit 1\n' > "$stub/lsof"; chmod +x "$stub/lsof"
  out="$( export PATH="$stub:$PATH"; "$WTCLEAN" "$root" --jobs 1 2>&1 )"
  if grep -q 'ガード無効' <<<"$out" \
     && ! grep -E '[[:space:]]reachable[[:space:]]' <<<"$out" | grep -q 'det-main' \
     && grep -E '[[:space:]]reachable[[:space:]]' <<<"$out" | grep -q 'old-merged'; then
    ok 'lsof が使えないとき detached は候補から外れる (fail closed)'
  else
    ng 'lsof が使えないとき detached は候補から外れる (fail closed)'
  fi
}

# gh が使えない (rc!=0) 状況を作り、has_open_pr が安全側 (open PR あり扱い) に
# 倒れて merged-else の誤爆が起きないことを見る。squash / closed-pr は空判定を
# 「その tier を付けない」方向にしか使わないので fail closed だが、has_open_pr は
# 唯一の否定ガードとして使われているため、空を「無い」と読むと危険側に倒れる。
assert_gh_fail_closed() {
  local root="$1" stub="$T/ghfailbin" out
  mkdir -p "$stub"
  printf '#!/usr/bin/env bash\nexit 1\n' > "$stub/gh"; chmod +x "$stub/gh"
  out="$( export PATH="$stub:$PATH"; "$WTCLEAN" "$root" --jobs 1 2>&1 )"
  local rc=$?
  if [[ -n "$out" && $rc -eq 0 ]] \
     && grep -q '安全側' <<<"$out" \
     && ! grep -E '[[:space:]](reachable|squash|merged-else|closed-pr|landed-pr)[[:space:]]' <<<"$out" \
          | grep -qF 'child-open'; then
    ok 'gh が失敗すると has_open_pr は安全側 (open扱い) に倒れ merged-else が誤爆しない'
  else
    ng 'gh が失敗すると has_open_pr は安全側 (open扱い) に倒れ merged-else が誤爆しない'
  fi
}

# fake gh: wt-clean が実際に使う呼び出しだけを再現する。
# GH_CLOSED / GH_MERGED / GH_OPEN に空白区切りでブランチ名を並べて挙動を制御する。
#
# -q のフィルタを必ず適用すること。実 gh は -q を適用した結果を出すので、
# JSON をそのまま出すと呼び出し側の `oid="$(gh ... -q '.[0].headRefOid')"` が
# PR の無いブランチでも "[]" という非空文字列を受け取り、[[ -n "$oid" ]] が
# 常に真になって判定が壊れる。
# --state による絞り込みも再現すること (tier2 は --state merged、
# has_open_pr は --state open で引く)。
make_fake_gh() {
  local d="$1"; mkdir -p "$d"
  cat > "$d/gh" <<'GHEOF'
#!/usr/bin/env bash
head=""; filter=""; want=""; prev=""
for a in "$@"; do
  case "$prev" in
    --head)  head="$a" ;;
    -q|--jq) filter="$a" ;;
    --state) want="$a" ;;
  esac
  prev="$a"
done

state=""
case " ${GH_MERGED:-} " in *" $head "*) state=MERGED ;; esac
case " ${GH_CLOSED:-} " in *" $head "*) state=CLOSED ;; esac
case " ${GH_OPEN:-} "   in *" $head "*) state=OPEN ;; esac

# --state <x> は該当する状態だけを返す (それ以外は空)
case "$want" in
  merged) [[ "$state" == MERGED ]] || state="" ;;
  open)   [[ "$state" == OPEN ]]   || state="" ;;
esac

oid=""
[[ -n "$state" ]] && oid="$(cat "$FAKE_GH_STATE/$head.oid" 2>/dev/null)"

case "$filter" in
  *headRefOid) printf '%s\n' "$oid" ;;
  *state)      printf '%s\n' "$state" ;;
  # 「該当 PR 無し」は実 gh でも成功 (rc=0) として出力が空になるだけ。
  # [[ ]] の結果をそのまま case 全体の終了ステータスにすると、state が空の
  # ときに素の [[ -n "$state" ]] (偽) が最後の実行コマンドになって rc=1 に
  # 化けてしまう (実 gh の挙動と食い違う) ので、明示的に true で締める。
  *number)     [[ -n "$state" ]] && printf '1\n'; true ;;
  *)           if [[ -n "$state" ]]; then
                 printf '[{"state":"%s","headRefOid":"%s"}]\n' "$state" "$oid"
               else
                 printf '[]\n'
               fi ;;
esac
GHEOF
  chmod +x "$d/gh"
}

# 合成リポジトリ。origin は同じ temp 配下の bare repo。
mkfixture() {
  local root="$1"
  local origin="$root/origin.git" repo="$root/repo"
  mkdir -p "$root"
  git init --quiet --bare -b main "$origin"

  git init --quiet -b main "$root/seed"
  git -C "$root/seed" config user.email t@example.com
  git -C "$root/seed" config user.name test
  echo base > "$root/seed/f"; git -C "$root/seed" add f
  git -C "$root/seed" commit --quiet -m base
  git -C "$root/seed" remote add origin "$origin"
  git -C "$root/seed" push --quiet origin main

  git clone --quiet "$origin" "$repo"
  git -C "$repo" config user.email t@example.com
  git -C "$repo" config user.name test
  echo "$repo"
}

# ケース: default の祖先にあるブランチ -> reachable
# 「base に乗っているだけのブランチ」。main は先に進んでいる。
case_reachable_branch() {
  local repo="$1" base
  base="$(git -C "$repo" rev-parse origin/main)"
  git -C "$repo" branch old-merged "$base"
  git -C "$repo" worktree add --quiet "$repo/.wt/old-merged" old-merged
  # main を先に進めて push (old-merged が真に祖先になる)
  echo more > "$repo/f2"; git -C "$repo" add f2
  git -C "$repo" commit --quiet -m advance
  git -C "$repo" push --quiet origin main
}

# ケース: open PR 相当 = 自分の upstream にしか含まれない -> 候補外
case_open_pr_branch() {
  local repo="$1"
  git -C "$repo" checkout --quiet -b open-pr origin/main
  echo x > "$repo/open"; git -C "$repo" add open
  git -C "$repo" commit --quiet -m open-work
  git -C "$repo" push --quiet -u origin open-pr
  git -C "$repo" checkout --quiet main
  git -C "$repo" worktree add --quiet "$repo/.wt/open-pr" open-pr
}

# ケース: .wt 配下に .git を持たないディレクトリ -> orphan
case_orphan() {
  local repo="$1"
  mkdir -p "$repo/.wt/leftover/build"; echo junk > "$repo/.wt/leftover/build/x"
}

# ケース: tier3 landed-pr (wt-clean のヘッダコメントに定義がある)。PR の中身は origin/main に
# 取り込み済みだが、ローカル tip はそこに到達しない (rebase で積み直した後、
# のようなケース)。PR 相当のコミットを origin/main に直接積んで headRefOid
# として記録し、ローカルブランチはその親から別内容で分岐させる —
# tip は headRefOid と一致せず (squash ではない)、headRefOid 自体は
# origin/main の祖先 (取り込み済み) という条件を満たす。
case_landed_pr_branch() {
  local repo="$1" parent pr_oid
  parent="$(git -C "$repo" rev-parse origin/main)"

  git -C "$repo" checkout --quiet main
  git -C "$repo" pull --quiet origin main
  echo pr-content > "$repo/pr-content"; git -C "$repo" add pr-content
  git -C "$repo" commit --quiet -m "pr work landed"
  git -C "$repo" push --quiet origin main
  pr_oid="$(git -C "$repo" rev-parse HEAD)"

  git -C "$repo" checkout --quiet -b reworked "$parent"
  echo reworked-content > "$repo/reworked-content"; git -C "$repo" add reworked-content
  git -C "$repo" commit --quiet -m "reworked after landing"
  git -C "$repo" checkout --quiet main
  git -C "$repo" worktree add --quiet "$repo/.wt/reworked" reworked

  echo "$pr_oid" > "$FAKE_GH_STATE/reworked.oid"
  export GH_MERGED="reworked"
}

# ケース: default ではなく別の feature ブランチに合流済み -> merged-else
# child の commit を parent に取り込んで parent だけ push する。
case_merged_else() {
  local repo="$1"
  git -C "$repo" checkout --quiet -b child origin/main
  echo c > "$repo/c"; git -C "$repo" add c
  git -C "$repo" commit --quiet -m child-work
  git -C "$repo" checkout --quiet -b parent
  git -C "$repo" push --quiet origin parent
  git -C "$repo" checkout --quiet main
  git -C "$repo" worktree add --quiet "$repo/.wt/child" child
}

# ケース: merged-else と同じ形 (取り込み先の親ブランチが push 済み) だが、
# 自分自身に open な PR が付いている -> スタック作業中なので候補外。
# 実データで見つかったバグの再現: 取り込み済み = 完了とは限らず、
# 両方の PR がまだ動いている途中のことがある。それを見ずに削除すると
# 作業中のブランチを壊す。
case_merged_else_open_pr() {
  local repo="$1"
  git -C "$repo" checkout --quiet -b child-open origin/main
  echo co > "$repo/co"; git -C "$repo" add co
  git -C "$repo" commit --quiet -m child-open-work
  git -C "$repo" push --quiet -u origin child-open
  git -C "$repo" checkout --quiet -b parent-open
  git -C "$repo" push --quiet origin parent-open
  git -C "$repo" checkout --quiet main
  git -C "$repo" worktree add --quiet "$repo/.wt/child-open" child-open
  export GH_OPEN="child-open"
}

# ケース: push はしているが追跡設定 (branch.<name>.remote) が無いブランチ、
# かつどこにも合流していない -> 候補外。
# 実データで見つかったバグの再現: kango-72-graphql は @{u} が空になるため、
# others_containing が自分自身のリモート追跡 ref (origin/<同名>) を除外
# できず、「自分の upstream 以外に含まれる」を満たしてしまい merged-else に
# 誤爆していた。push.autoSetupRemote が有効な環境では素の push でも
# 追跡設定がついてしまうため、明示的に外して未追跡の状態を作る。
case_untracked_pushed() {
  local repo="$1"
  git -C "$repo" checkout --quiet -b untracked origin/main
  echo u > "$repo/u"; git -C "$repo" add u
  git -C "$repo" commit --quiet -m untracked-work
  git -C "$repo" push --quiet origin untracked
  git -C "$repo" config --unset branch.untracked.remote 2>/dev/null
  git -C "$repo" config --unset branch.untracked.merge 2>/dev/null
  git -C "$repo" checkout --quiet main
  git -C "$repo" worktree add --quiet "$repo/.wt/untracked" untracked
}

# ケース: push 済みで PR が CLOSED -> closed-pr
# 既定では候補外 (要確認枠)、--include-closed-pr で候補に入る。
case_closed_pr() {
  local repo="$1" tip
  git -C "$repo" checkout --quiet -b dead-pr origin/main
  echo d > "$repo/d"; git -C "$repo" add d
  git -C "$repo" commit --quiet -m dead-work
  git -C "$repo" push --quiet -u origin dead-pr
  tip="$(git -C "$repo" rev-parse HEAD)"
  echo "$tip" > "$FAKE_GH_STATE/dead-pr.oid"
  git -C "$repo" checkout --quiet main
  git -C "$repo" worktree add --quiet "$repo/.wt/dead-pr" dead-pr
}

# ケース: worktree のディレクトリだけ消えて登録が残っている -> prunable
case_prunable() {
  local repo="$1"
  git -C "$repo" worktree add --quiet --detach "$repo/.wt/gone" origin/main
  rm -rf "$repo/.wt/gone"
}

# ケース: origin/main の祖先にある detached -> reachable
case_reachable_detached() {
  local repo="$1"
  git -C "$repo" worktree add --quiet --detach "$repo/.wt/det-main" origin/main
}

# ケース: どの ref からも到達不能なコミットを持つ detached -> 候補外
# (今日の 2ce8 に相当。worktree を消すとコミットが到達不能になるので消してはいけない)
case_detached_unique() {
  local repo="$1"
  git -C "$repo" worktree add --quiet --detach "$repo/.wt/det-uniq" origin/main
  git -C "$repo/.wt/det-uniq" config user.email t@example.com
  git -C "$repo/.wt/det-uniq" config user.name test
  echo uniq > "$repo/.wt/det-uniq/u"
  git -C "$repo/.wt/det-uniq" add u
  git -C "$repo/.wt/det-uniq" commit --quiet -m "unique work"
}

# ケース: 稼働中プロセスが cwd として掴んでいる detached -> in-use (候補外)
# sleep の cwd をその worktree にして掴ませる。PID は caller が kill する。
# stdout/stderr は /dev/null に逃がす: 継承したままだと、このテストスクリプト
# 全体の出力をパイプで受け取っている呼び出し元 (この out="$(...)" 自体や、
# 外側のハーネス) が、バックグラウンドの sleep がパイプの書き込み端を握ったまま
# 終了しないせいで sleep 120 が切れるまで戻ってこなくなる。
case_in_use() {
  local repo="$1"
  git -C "$repo" worktree add --quiet --detach "$repo/.wt/det-busy" origin/main
  ( cd "$repo/.wt/det-busy" && exec sleep 120 ) >/dev/null 2>&1 &
  echo $!
}

# ケース: repo2 の worktree (inner) が repo1 の worktree (outer) の中にある。
# inner には未 push のコミットを積む。has_nested_repo が outer を候補から
# 外すので、outer が (tier1 reachable 相当で) 削除候補一覧に混じって --apply で
# 丸ごと消え、inner の未 push コミットがどの ref からも到達不能なまま失われる、
# ということが起きないことを固定する CRITICAL の回帰テスト。
# 出力は「repo1<改行>repo2」の2行、呼び出し側で read で受ける。
case_nested_repo() {
  local root="$1"
  local repo1 repo2 inner
  repo1="$(mkfixture "$root/n1")"
  repo2="$(mkfixture "$root/n2")"
  inner="$repo1/.wt/outer/inner"
  git -C "$repo1" worktree add --quiet --detach "$repo1/.wt/outer" origin/main
  git -C "$repo2" worktree add --quiet --detach "$inner" origin/main
  git -C "$inner" config user.email t@example.com
  git -C "$inner" config user.name test
  echo unpushed > "$inner/u"; git -C "$inner" add u
  git -C "$inner" commit --quiet -m unpushed-work
  printf '%s\n%s\n' "$repo1" "$repo2"
}

run_wtclean() {
  local root="$1"; shift
  ( export PATH="$FAKE_GH_BIN:$PATH"; "$WTCLEAN" "$root" "$@" 2>&1 )
}

main() {
  # T は EXIT trap から参照するのでグローバルにする。local にすると trap の
  # 展開時 (main を抜けた後) に未割り当てとなり、set -u で落ちて後始末されない。
  T="$(mktemp -d)"
  trap 'rm -rf "$T"' EXIT
  export FAKE_GH_STATE="$T/ghstate"; mkdir -p "$FAKE_GH_STATE"
  export FAKE_GH_BIN="$T/fakebin"; make_fake_gh "$FAKE_GH_BIN"

  local repo; repo="$(mkfixture "$T/fx")"
  case_reachable_branch "$repo"
  case_open_pr_branch  "$repo"
  case_orphan          "$repo"
  case_landed_pr_branch "$repo"
  case_merged_else     "$repo"
  case_merged_else_open_pr "$repo"
  case_untracked_pushed "$repo"
  case_reachable_detached "$repo"
  case_detached_unique    "$repo"
  case_closed_pr          "$repo"
  export GH_CLOSED="dead-pr"
  case_prunable           "$repo"

  local busy_pid; busy_pid="$(case_in_use "$repo")"
  trap 'rm -rf "$T"; kill '"$busy_pid"' 2>/dev/null' EXIT

  echo "== dry-run"
  local out; out="$(run_wtclean "$T/fx" --jobs 1)"
  local rc=$?

  assert_tier          "$out" old-merged reachable
  assert_not_candidate "$out" "$rc" open-pr
  assert_reported      "$out" 'leftover' 'orphan として報告される'

  # landed-pr は要確認 (report_review 関数) にだけ出て、削除候補一覧 (D_* 配列) には
  # 出ないはず (worker() の cand ルーティングで landed-pr は R_PATH 行きになる)。
  assert_reported      "$out" 'reworked' 'reworked (landed-pr) は要確認として報告される'
  assert_not_candidate "$out" "$rc" reworked

  assert_tier          "$out" child merged-else
  assert_not_candidate "$out" "$rc" child-open
  assert_not_candidate "$out" "$rc" untracked

  # detached はブランチ名が無いので候補行のパス列で照合する。
  assert_tier          "$out" '.wt/det-main' reachable
  assert_not_candidate "$out" "$rc" '.wt/det-uniq'

  assert_not_candidate "$out" "$rc" '.wt/det-busy'
  assert_reported      "$out" 'in-use' 'in-use として報告される'

  assert_not_candidate "$out" "$rc" dead-pr
  assert_reported      "$out" 'closed-pr' 'closed-pr が要確認枠に出る'

  local out2; out2="$(run_wtclean "$T/fx" --jobs 1 --include-closed-pr)"
  assert_tier "$out2" dead-pr closed-pr

  assert_reported "$out" 'prunable' 'prunable として報告される'
  if git -C "$repo" worktree list --porcelain | grep -q 'prunable'; then
    ok 'dry-run は prune を実行しない'
  else
    ng 'dry-run は prune を実行しない'
  fi

  assert_fail_closed "$T/fx"
  assert_gh_fail_closed "$T/fx"

  echo "== nested repo (CRITICAL 回帰)"
  local repo1 repo2
  { read -r repo1; read -r repo2; } < <(case_nested_repo "$T/nest")
  local unpushed_sha; unpushed_sha="$(git -C "$repo1/.wt/outer/inner" rev-parse HEAD)"

  local nest_out; nest_out="$(run_wtclean "$T/nest" --jobs 1)"
  local nest_rc=$?
  assert_not_candidate "$nest_out" "$nest_rc" outer
  assert_reported      "$nest_out" '別リポジトリ' \
    '入れ子の別リポジトリを含む worktree は要確認として報告される'

  run_wtclean "$T/nest" --jobs 1 --apply >/dev/null 2>&1
  if [[ -d "$repo1/.wt/outer" ]] \
     && [[ "$(git -C "$repo1/.wt/outer/inner" rev-parse HEAD 2>/dev/null)" == "$unpushed_sha" ]]; then
    ok '入れ子の別リポジトリを含む候補は --apply でも削除されず、未 push コミットが残る'
  else
    ng '入れ子の別リポジトリを含む候補は --apply でも削除されず、未 push コミットが残る'
  fi

  echo
  printf 'pass=%d fail=%d\n' "$PASS" "$FAIL"
  [[ $FAIL -eq 0 ]]
}

main "$@"
