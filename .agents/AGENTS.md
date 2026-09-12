## Priorities and Scope

- Treat the user's stated goal, constraints, and confirmed preferences as authoritative. Skills guide execution but do not override explicit user instructions.
- Infer routine details from the repository and prior conversation, and continue through implementation and proportionate verification. Ask only when a missing choice would materially change the goal, scope, constraints, authority, or externally visible result.
- When a materially better alternative exists, state it briefly with the important trade-off. Proceed with the user's approach unless adopting the alternative is minor, reversible, and within scope.
- Prefer the smallest sufficient solution. Apply YAGNI, KISS, and DRY; do not add compatibility shims or fallback paths unless they add no meaningful complexity.

## Code Quality

- Default to no code comment. Add one concise comment only when a non-obvious reason or constraint cannot be expressed by the code itself; do not narrate the implementation or preserve tangential history.
- Write documentation for the reader's decision or next action. Explain why, use examples when they clarify a constraint, and omit details recoverable from the code or automation.
- Make invalid states structurally impossible with types, schemas, permissions, or automated checks when practical.
- Do not put mechanically verifiable work such as lint, tests, CI, or hooks in human checklists. Automate it and reserve checklists for judgment that cannot be encoded reliably.
- Add focused tests for new behavior and bug fixes. Do not test trivial forwarding functions.
- Auto-fix lint and formatting issues in files you touched without including unrelated formatting-only changes.

## Investigation and Verification

- For an experimental change made to diagnose a cause, state one hypothesis, change one relevant variable, test it, and revert the change if the result does not support the hypothesis.
- Verify changes in proportion to their risk and affected surface. Run affected tests first; broaden checks only when shared behavior, build configuration, or repository policy warrants it.
- Keep working through failures caused by the requested change and rerun affected checks. Stop when completion requires a material product decision, new authority, unavailable external state, or when the remaining failure is demonstrated to be unrelated.
- For substantial reviews, use an independent context that receives the diff and specification without the implementer's reasoning. Routine or mechanical edits do not require a separate reviewer.

## Shared Writing

- Keep PR bodies, errors, review comments, and shared documentation self-contained but concise. Do not assume the reader will reconstruct missing context, and do not include details that change neither understanding nor action.
- In notes and memos, avoid horizontal rules and bold emphasis; use plain text and headings for structure.
- Do not hard-wrap prose in Markdown or shared prose artifacts, including notes, PR bodies, skill files, review comments, and design documents. Let the renderer wrap paragraphs; preserve structural line breaks for lists, tables, and code blocks. Commit messages are exempt.

## Japanese Writing Style

- In Japanese prose, use natural Japanese translations or established katakana terms for ordinary words. Do not attach Japanese particles, conjugation, or copulas directly to an untranslated English word, such as `resolve する`, `fresh な`, or `read が返る`.
- When uncertain, apply the attachment test: if a Japanese particle or conjugation attaches directly to a single English word, translate or transliterate that word. For example, write `低い重大度`, `拒否する`, `解決済みの対象`, and `新しいリクエスト`, not `低 severity`, `refuse する`, `resolved target`, or `fresh な request`.
- Preserve English when translation would make the target ambiguous: proper names, API names, functions, types, libraries, paths, and exact identifiers. Also preserve established compound concepts whose meaning would blur under a one-word translation, such as YAGNI, pathological correctness, and human-in-the-loop.
- Established technical nouns such as API, URL, commit, merge, and CLI may remain as nouns. When used as Japanese verbs, transliterate them, such as `コミットする`, rather than writing `commit する`.
- Before sending Japanese prose, scan remaining English words and translate any that fail these exceptions. Prefer the more readable choice between katakana and a Japanese compound, and avoid long chains of katakana terms.

## Notes and Evidence

- Save a Markdown note when research, investigation, a plan, or a progress record has durable reuse value. Do not create a note for a short answer or a routine edit.
- Use the first existing location: project-root `.matsuyoshi/` or `.matsuyoshi30/`, then user-level `~/.matsuyoshi/` or `~/.matsuyoshi30/`, then the tool's memory directory. These directories are globally gitignored.
- When conclusions depend on hands-on measurement, a live system, or inspected artifacts that are not otherwise reproducible, add `Appendix: Work Record` immediately after the measurement. Record the environment, setup, exact commands or SQL, raw values, limitations, and temporary-resource cleanup; omit trial-and-error that does not support the conclusion.
- In PRs, issues, and shared design documents, link code references to GitHub at the current full SHA. Use plain `path:line` for personal notes, untracked or modified files, commits not present on a remote branch, or non-GitHub remotes.
