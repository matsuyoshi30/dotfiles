---
name: design-doc
description: Use when the user wants to write, scaffold, or review a design doc / RFC / 設計ドキュメント for a feature, architecture change, schema change, or API design that warrants written design review. Triggers on phrases like "write a design doc", "design doc 書いて", "/design-doc", or when starting to plan a non-trivial change worth documenting before implementation.
---

# Design Doc Skill

Scaffold and review design docs using `template.md`.

## How to use

1. If it's unclear what the doc is about, ask once for a one-line description of the feature or change. Don't pile on clarifying questions.
2. Read `template.md` and use its structure as the skeleton. Keep every section; for anything you can't fill yet, leave `_TBD_` so open items stay visible rather than silently dropped.
3. Fill each section concretely:
   - Define ambiguous or domain-specific terms in the Glossary so reviewers share the same vocabulary.
   - Cover the data model and any migration concerns: schema changes, backfill, rollback, and whether the change can ship without downtime.
   - Cover the API/interface surface: new or changed endpoints, naming, error handling, and backward compatibility.
   - Call out security, privacy, and access-control impact when relevant.
4. Do not omit Alternatives Considered, Risks / Trade-offs, or Open Questions — the value of a design review lives there.
5. If a tracker issue (Linear, etc.) is referenced, link it in the metadata. Never fabricate a URL; leave the field blank if unknown.

## Output

- Produce a single Markdown document. Write it in the language the user is working in (default to their language).
- Don't state unresolved decisions as if they're settled. Put anything you couldn't decide into Open Questions.
