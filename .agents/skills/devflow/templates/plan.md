# PLAN

## Goal
<!-- 1-2 sentences: what we're building and why -->

## Constraints
<!-- What NOT to do, rules to follow, guardrails -->

## Definition of Done
<!-- REQUIRED — do not leave empty. List verifiable, concrete completion criteria:
     observable behaviors, test conditions, regression guards. Each item must be
     checkable (pass/fail), not aspirational. -->

## Approach
<!-- Selected approach and rationale -->

## Steps
<!-- One entry per implementer-ready unit of work. Each step must be small enough
     to dispatch as a single implementer-agent run.

     Required metadata per step:
       - files: [...]   OR   scope: <glob>      — what the step is allowed to touch
       - depends_on: [<ids of prior steps>]     — empty list when independent

     Granularity targets (see reference/execution-mode.md):
       - ≤ 5 files per step (or scope-bounded)
       - No two steps mutate the same function signature
       - If a step would need a Decision Record (DR) to start, split it further

     `files: TBD` is allowed only when the file set genuinely depends on a prior
     step's output (e.g., placement decided by step 3). Add a 1-line reason. TBD
     steps disqualify that segment from PER_TASK dispatch — see execution-mode.md. -->

1. **{title}**
   - files: [<relative/path/a>, <relative/path/b>]
   - depends_on: []
   - {one paragraph or short bullet list of what to do}

2. **{title}**
   - scope: <src/area/**>
   - depends_on: [1]
   - ...

## Spike Learnings
<!-- Populated after spike phase -->

## Decision Log
<!-- DRs accumulated during execution -->

## Dependency Inventory (refactor / move tasks only)
<!-- Required for move/rename/extract tasks. Omit otherwise.

(a) Symbols being moved: enumerate
(b) All classes the moved code imports (including same-module imports)
(c) Sibling files that reference the moved code by bare name (to detect same-package implicit imports)
(d) Helper / utility functions the moved code uses (check for cross-module duplicates)

For each entry, mark one of: "move", "inline", "promote to shared", "leave in place". -->

---

## Example (delete before use)

```markdown
# PLAN

## Goal
Add CSV export to the /reports endpoint so users can download report data in spreadsheet-compatible format.

## Constraints
- Do not change existing JSON response format
- Use streaming for files > 1MB to avoid memory spikes
- No new dependencies — use stdlib encoding/csv

## Definition of Done
- GET /reports?format=csv returns valid CSV with headers
- Existing JSON responses unchanged (regression test)
- Unit test covers: empty data, special characters in fields, streaming threshold

## Approach
Add a format query parameter to the existing handler. When format=csv, pipe rows through encoding/csv writer directly to ResponseWriter. Reuse the same query logic — only the serialization layer changes.

## Steps

1. **Add CSVRow() method to ReportRow**
   - files: [internal/report/row.go, internal/report/row_test.go]
   - depends_on: []
   - Expose existing unexported fields via a CSVRow() []string method matching JSON key order.

2. **Wire format query parameter into handler**
   - files: [internal/httpapi/reports.go, internal/httpapi/reports_test.go]
   - depends_on: [1]
   - Parse `?format=csv`. On csv, stream via encoding/csv.Writer wrapped in bufio. Default to JSON.

3. **Add streaming threshold + regression tests**
   - scope: internal/httpapi/**
   - depends_on: [2]
   - Buffer rows under 1MB; flush per row above. Tests cover empty data, comma/quote in fields, threshold crossover.

## Spike Learnings
- encoding/csv.Writer flushes per-call; wrap in bufio for streaming (discovered via spike)
- Existing ReportRow struct has unexported fields — CSVRow() method needed (covered by step 1)

## Decision Log
- DR-1: Header naming — use snake_case field names (e.g. created_at) to match JSON keys. User chose this over Title Case for consistency.
```
