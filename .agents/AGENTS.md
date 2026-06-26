## Code Quality

- Add clear comments explaining complicated business logic
  - Only write comments for non-obvious things not expressed by the code itself, and keep them concise
  - Avoid redundant comments that merely restate what the code already shows
- Generate comprehensive documentation
  - Write WHY not WHAT
- Create examples in documentation
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

## Notes Style

- Avoid horizontal rules (`---`) and bold emphasis (`**`) in notes and memos
- Use plain text and headings (`#`) for structure instead

## Japanese Writing Style

- 日本語で説明するときは、英単語を生のまま文中に混ぜない
  - 自然な日本語訳や定着したカタカナ語がある語は、そちらで書く
    - 例: `bounded retry` → 回数制限付きの再試行 / 上限付きリトライ、`resolve` → 解決 / 確定、`fresh な request` → 新しいリクエスト、`合流元 read` → 合流元の読み取り
  - 英単語を助詞や活用に直接つなぐ書き方(「resolve する」「fresh な」「read が返る」「bounded retry します」)をしない
- 英語のまま残してよいのは次の場合に限る
  - 固有名詞・API 名・関数名・型名・ライブラリ名・ファイルパスなど、訳すと指示対象がぶれるもの(例: `ProgressNote`, `utils/refetchUntil`)
  - 業界で定着しており日本語化するとかえって読みにくい語(例: API, URL, commit, merge, CLI)
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

@RTK.md
