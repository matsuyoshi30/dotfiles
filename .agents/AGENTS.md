## Code Quality

- Add clear comments explaining complicated business logic
  - Only write comments for non-obvious things not expressed by the code itself, and keep them concise
- Generate documentation that explains WHY not WHAT, with examples
- Auto-fix all linting/formatting issues
- Write tests for new features and bug fixes
  - Don't test trivial functions like just call other utilities

## Hypothesis Testing

- Test only one hypothesis per change
- Explain the hypothesis and change details beforehand
- Validate the hypothesis through testing
- Revert the change if testing shows no effect

## Agent Guidelines

- Always prefer simplicity over pathological correctness
- YAGNI, KISS, DRY
- No backward compatibility shims or fallback paths unless they come free without adding cyclomatic complexity

## Working Principles

- Write for the reader in anything shared with others (PRs, error messages, docs, comments)
  - Don't write vague messages that assume the reader will dig through the codebase; make the text understandable on its own
  - But "writing everything is kinder" is an illusion. Cut details that don't change the reader's understanding or next action. Self-contained is not the same as exhaustive
- Don't put things in checklists that automation can handle
  - Push anything mechanically verifiable (lint, tests, CI, hooks) into automation; keep only items requiring human judgment in checklists
- Prefer "structurally impossible" over "be careful"
  - Don't rely on vigilance to prevent mistakes; make invalid states unrepresentable via types, schemas, permissions, and automated checks
- Review in a structurally independent context
  - A review that inherits the implementation context reproduces the implementer's assumptions. Hand only the diff and the spec to a party without that context, such as a separate session or subagent

## Notes Style

- Avoid horizontal rules (`---`) and bold emphasis (`**`) in notes and memos
- Use plain text and headings (`#`) for structure instead

## Japanese Writing Style

- 日本語で説明するときは、英単語を生のまま文中に混ぜない
  - 自然な日本語訳や定着したカタカナ語がある語は、そちらで書く
    - 例: `bounded retry` → 回数制限付きの再試行 / 上限付きリトライ、`resolve` → 解決 / 確定、`fresh な request` → 新しいリクエスト、`合流元 read` → 合流元の読み取り
  - 英単語を助詞や活用に直接つなぐ書き方(「resolve する」「fresh な」「read が返る」「bounded retry します」)をしない
- 残すか訳すか迷ったら「貼り付きテスト」で判定する
  - 英単語の直後に日本語の助詞・送り仮名・コピュラが直接貼り付くなら、その語は一語で訳せる普通語なので訳す
    - NG例: 「低 severity の」「refuse する」「resolved target が」「fresh な」 → 「低い重大度の」「拒否する」「解決済みの対象が」「新しい」
  - 概念・定型句として名詞のかたまりで独立し、助詞がかたまりの外側に付くだけなら残してよい
    - OK例: 「YAGNI の観点で」「pathological correctness を避ける」「human-in-the-loop なので」
  - 区別の軸は「業界で定着しているか」ではなく「一語で訳せる普通語が文法的に日本語と直接結合しているか」
- 英語のまま残してよいのは次の場合に限る
  - 固有名詞・API 名・関数名・型名・ライブラリ名・ファイルパスなど、訳すと指示対象がぶれるもの(例: `utils/refetchUntil`)
  - 一語の訳語に置き換えると意味がぼやける複合概念・定型句(例: YAGNI, pathological correctness, human-in-the-loop)
  - 業界で定着しており日本語化するとかえって読みにくい語(例: API, URL, commit, merge, CLI)
- 日本語の回答を出す直前に、文中の英単語を一語ずつ洗い出し、貼り付きテストで残してよい側に該当しない語をすべて訳してから出力する
- カタカナにすべきか漢字熟語にすべきかは読みやすさで判断し、過度なカタカナ連結も避ける

## Notes Directory

- Automatically save research, investigation, summary results, plans and progress tracking as markdown files
- Save location (fallback chain)
  1. `.matsuyoshi/` or `.matsuyoshi30/` at project root (if exists)
  2. `$HOME/.matsuyoshi/` or `$HOME/.matsuyoshi30/` for cross-project notes (if exists)
  3. Tool's built-in memory directory
- `.matsuyoshi` / `.matsuyoshi30` are globally gitignored

## Code References

- When writing code references (file:line) into notes, PR bodies, or Linear comments,
  link them as GitHub permanent links:
  `[path/file.kt:123](https://github.com/{owner}/{repo}/blob/{full SHA}/path/file.kt#L123)`
  (line range: `#L10-L20`)
- Resolve owner/repo from `git remote get-url origin` and the SHA from
  `git rev-parse HEAD` (once per repo per session)
- Keep plain `file:line` when the file is untracked, modified relative to HEAD
  (`git diff HEAD -- <file>` non-empty; the permalink would point at stale lines),
  or the remote is not GitHub
