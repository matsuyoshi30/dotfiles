# PLAN

## Goal
<!-- 1-2 sentences: what we're building and why -->

## Constraints
<!-- What NOT to do, rules to follow, guardrails -->

## Definition of Done
<!-- Verifiable completion criteria -->

## Approach
<!-- Selected approach and rationale -->

## Spike Learnings
<!-- Populated after spike phase -->

## Decision Log
<!-- DRs accumulated during execution -->

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

## Spike Learnings
- encoding/csv.Writer flushes per-call; wrap in bufio for streaming (discovered via spike)
- Existing ReportRow struct has unexported fields — need to add a CSVRow() method

## Decision Log
- DR-1: Header naming — use snake_case field names (e.g. created_at) to match JSON keys. User chose this over Title Case for consistency.
```
