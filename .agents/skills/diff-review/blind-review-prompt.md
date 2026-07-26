You are reviewing the following diff. You do NOT know the plan behind it. Do not
read plan files or design notes: anything under `plans/`, `.matsuyoshi*/`,
`.devflow/`, `docs/superpowers/plans/`, any `WORKLOG.md` / `exploration.md` /
`retrospective.md`, or files whose name contains `plan`.
You MAY read other existing repository files to check how the changed code uses
unchanged code. When you use Grep or Glob, always scope them to explicit target
paths — never run a repo-wide unscoped search: search results quote matching
lines, so an unscoped Grep hands you the plan's content without "reading" it.
If a result from a forbidden path ever appears, discard it unused. Work alone,
from the diff and the repository files only: do not consult other agents,
sessions, or external services.

Working directory: {cwd}

Annotated diff (each hunk is prefixed with its id like `[h001]`):

{annotated_diff}

Do the following:

1. Group hunks by intent, not by file order (e.g. a rename and its import fixups
   are one group). Reference hunks by their ids. If one hunk carries two intents
   (e.g. a rename and a new function in the same hunk), assign it to the group of
   its higher-risk intent — do not split hunks. But when a purely mechanical/safe
   hunk (documentation, changelog, comment, or formatting) merely accompanies a
   riskier code change, keep it as its own `safe` group rather than merging it into
   the risky group, so its low risk stays visible. (This split is between separate
   hunks; a single hunk carrying two intents still follows the higher-risk rule.)
2. Assign each group a risk level. A group's risk is the highest risk implied by
   any change it contains. Use these definitions:
   - `safe`: mechanical, no behavior change (pure rename propagation, formatting,
     comment/doc text, moving code unchanged).
   - `low`: a contained behavior change whose effect is obvious and localized
     (a config flag flip, a small additive helper with clear semantics).
   - `attention`: could be wrong or needs a human to look — a possible bug, misuse
     of an existing function, non-obvious semantics, or anything touching
     security or data integrity.
3. Write a short `intent` sentence per group describing what the change does.
4. For each concern, add a finding with `severity` (`info`/`warning`/`critical`),
   a one-line `summary`, a `location` (`file:line`), and a `detail`. Prioritise
   findings only defensible by reading existing code (e.g. misuse of an existing
   function).

Output exactly one JSON object in a ```json fenced block, matching this schema:

```json
{
  "groups": [
    {
      "id": "g1",
      "title": "...",
      "intent": "...",
      "risk": "attention",
      "hunk_ids": ["h001", "h002"],
      "findings": [
        {
          "id": "f1",
          "severity": "warning",
          "summary": "...",
          "location": "src/foo.ts:109",
          "detail": "..."
        }
      ]
    }
  ]
}
```

List groups highest-risk first. Every hunk id in the annotated diff must appear
in exactly one group. Every group must carry a `findings` array — write `[]`
when a group has no findings, never omit the key.
