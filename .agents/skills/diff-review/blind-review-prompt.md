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

Annotated diff. Each hunk starts with a `### [h001] <file>` header that also lists
the line numbers this hunk actually changed. Every body line is `<marker><line>| <code>`:

- `+124| ...` — a line this diff ADDED (124 is its line number in the file now)
- `-45| ...` — a line this diff DELETED (45 is its line number in the old file)
- `ctx120| ...` — UNCHANGED surrounding code, shown only as context. This diff did
  not touch it. Most of what follows is `ctx`, because context is deliberately wide.

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
   a one-line `summary`, a `location` (`file:line`), a `detail`, and an `anchor`.
   Prioritise findings only defensible by reading existing code (e.g. misuse of an
   existing function).

   `anchor` is the single annotated-diff line the finding is about, copied verbatim
   as `<hunk_id>:<marker><line>` — `"h003:+124"`, `"h003:-45"`, `"h003:ctx120"`.
   Before writing it, look back at that exact line and confirm the marker.

   Attribution rule — this is the one error to avoid. Reading the surrounding `ctx`
   code is encouraged and is the main reason you have repo access, but a `ctx` line
   is code that already existed. If your concern is about code that appears only on
   `ctx` lines, you may NOT describe it as added, changed, introduced, removed, or
   broken by this diff. Either:
   - re-anchor to the `+`/`-` line that genuinely causes the problem (e.g. the new
     call site is wrong, even though the function it calls is unchanged), or
   - keep it as an observation about pre-existing code: set `"pre_existing": true`,
     anchor at the `ctx` line, and word `summary`/`detail` as "the existing X does
     Y", never "this change does Y".

   `pre_existing` is optional and omitted above because that example concerns an
   added line. Write `"pre_existing": true` on any finding whose defect lives in
   unchanged code, even when you anchor it to a `+`/`-` line; omit the key otherwise.

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
          "anchor": "h001:+109",
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

A group's `risk` describes what this diff changed. A `pre_existing` finding does
not raise it: unchanged code being imperfect is not a risk the diff introduced.
