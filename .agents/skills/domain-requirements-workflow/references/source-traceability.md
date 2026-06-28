# Source Traceability

## Purpose

Use this reference before reading documents, interviewing stakeholders, or summarizing findings. The workflow depends on preserving an audit trail from final claims back to concrete source material.

## Core Rule

Record the source first, then extract evidence, then shape claims.

Do not skip directly from source material to polished requirements.

Evidence comes only from real sources: real documents, real stakeholder conversations, and direct observation. Model-generated persona dry-runs produce hypotheses and questions, never evidence — they never get a `source_id` and never enter the evidence log.

## Required Files

- `source-ledger.json`
- `evidence-log.json`

Claim-to-evidence traceability is not a separate file. It lives inside each need and requirement record (`supported_by`, `need_ids`) and is rendered as a mapping in the final document.

## Source Ledger

Register every source with at least:

- `source_id`
- `type`
- `title`
- `author_or_speaker`
- `captured_at`
- `location`
- `reliability`
- `access_scope`

Source types:

- `document`
- `conversation`
- `observation`
- `web` (a page or document found by search; also add `url`, `search_query`, `accessed_at`, and `url_verified`)

Web sourcing rules:

- The web is a real source only for general-domain facts, never for this organization's specific operations or final requirements.
- A web finding is a hypothesis by default. Admit it as general-domain evidence only when it comes from an authoritative named source (standards body, regulation, formal specification, primary documentation). Rate `reliability` by that authority, not by how well it fits your expectation.
- Verify the URL resolves before logging the source. Fetch it with the runtime's fetch tool and confirm it resolves (e.g. returns 200, not a 404 or dead host), then record the outcome in `url_verified` (for example `200 @ 2026-06-28`). A plausible-looking URL is not proof it exists — models hallucinate URLs, so an unfetched URL is a fabrication risk. If the URL does not resolve, or no fetch tool is available, do not register it; keep the finding as a hypothesis until a resolvable URL is confirmed. This checks existence only; when the URL resolves but the content cannot be read (e.g. a PDF you cannot render), still log the source but lower its `reliability` and note the gap, rather than dropping it.
- Always record `search_query` and `accessed_at` so a human can audit what was searched and what was excluded — this is the only check on selection bias.
- Conflicting web sources are kept as conflicts, not silently resolved to the one you preferred.

Example:

```json
{
  "source_id": "S-001",
  "type": "document",
  "title": "Operations Manual v3",
  "author_or_speaker": "Operations Team",
  "captured_at": "2026-06-28T13:32:00+09:00",
  "location": "work/requirements-run/interview-notes/ops-manual-v3.md",
  "reliability": "high",
  "access_scope": "internal"
}
```

Web source example (note the extra `url`, `search_query`, `accessed_at`, `url_verified`):

```json
{
  "source_id": "S-002",
  "type": "web",
  "title": "Operations standard, Section 4",
  "author_or_speaker": "Standards Body",
  "captured_at": "2026-06-28T13:40:00+09:00",
  "location": "ops-standard §4",
  "url": "https://standards.example.org/ops/v3#sec-4",
  "search_query": "ops standard exception approval",
  "accessed_at": "2026-06-28T13:40:00+09:00",
  "url_verified": "200 @ 2026-06-28",
  "reliability": "high",
  "access_scope": "public"
}
```

## Evidence Log

Split each important observation into one meaning unit. Keep:

- `evidence_id`
- `source_id`
- `locator`
- `summary`
- `classification`
- `captured_by`
- `captured_at`
- `confidence`

Classifications:

- `fact`
- `inference`
- `request`

Example:

```json
{
  "evidence_id": "E-001",
  "source_id": "S-001",
  "locator": "chapter-3 / exception handling",
  "summary": "Exception requests require double approval and often wait two extra days.",
  "classification": "fact",
  "captured_by": "researcher",
  "captured_at": "2026-06-28T13:35:00+09:00",
  "confidence": "high"
}
```

## Claim Traceability

Every accepted need and requirement carries its own links to supporting evidence inside its record (`supported_by`, and `need_ids` for requirements) — see the need and requirement record shapes in [prompts-and-state.md](prompts-and-state.md). Do not duplicate these links into a separate claims file; the document generator walks the records to render the claim-to-evidence mapping.

When a claim still lacks real-source evidence, keep its `status` at `draft` and exclude it from the confirmed section of the final document.

## Non-Negotiable Rules

1. Register a source immediately after first reference.
2. Keep a locator to the original position or utterance.
3. Separate fact, inference, and request.
4. Leave unsupported claims in draft status.
5. Preserve conflicts rather than flattening them.
6. Require every major need and requirement to trace to evidence.
7. Never log a model-generated persona statement as evidence. It is a hypothesis until a real source confirms it.
8. A web finding is a hypothesis unless it comes from an authoritative named source, and is general-domain only — never an org-specific confirmed requirement without a real stakeholder. Record its query and access date.
9. Verify a web URL resolves (fetch it) before logging it. Never cite a URL you have not confirmed exists; record the outcome in `url_verified`.

## Review Checklist

Before finishing a run, confirm:

- every important source has a `source_id`
- every web source carries a `url` and a `url_verified` result (or is dropped)
- every major claim has `supported_by`
- evidence identifiers resolve to real entries
- conflicts and gaps remain visible
