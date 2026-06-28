# Source Traceability

## Purpose

Use this reference before reading documents, interviewing stakeholders, or summarizing findings. The workflow depends on preserving an audit trail from final claims back to concrete source material.

## Core Rule

Record the source first, then extract evidence, then shape claims.

Do not skip directly from source material to polished requirements.

## Required Files

- `source-ledger.json`
- `evidence-log.json`
- `claim-traceability.json`

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

Store every accepted need and requirement with direct links to supporting evidence.

Example:

```json
{
  "claim_id": "R-003",
  "claim_type": "requirement",
  "statement": "Show exception status and blocking approver in one place.",
  "derived_from": [
    "N-001"
  ],
  "supported_by": [
    "E-001",
    "E-010"
  ],
  "status": "supported"
}
```

## Non-Negotiable Rules

1. Register a source immediately after first reference.
2. Keep a locator to the original position or utterance.
3. Separate fact, inference, and request.
4. Leave unsupported claims in draft status.
5. Preserve conflicts rather than flattening them.
6. Require every major need and requirement to trace to evidence.

## Review Checklist

Before finishing a run, confirm:

- every important source has a `source_id`
- every major claim has `supported_by`
- evidence identifiers resolve to real entries
- conflicts and gaps remain visible
