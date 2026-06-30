---
name: design-doc
description: Use when the user wants to write or scaffold a design doc / RFC / 設計ドキュメント for a feature, architecture change, schema change, or API design that warrants a written design before implementation. Triggers on phrases like "write a design doc", "design doc 書いて", "/design-doc", or when starting to plan a non-trivial change worth documenting before implementation.
---

# Design Doc Skill

Scaffold design docs using `template.md`.

## How to use

1. If it's unclear what the doc is about, ask once for a one-line description of the feature or change. Don't pile on clarifying questions.
2. Read `template.md` and use its structure as the skeleton. Follow the template's own keep/delete rule — do not keep every section:
   - Always write the **Core** section.
   - For each **Optional** section, read its 〈When you need it〉 note. If it applies but you can't fill it in yet, leave `_TBD_` so the open item stays visible. If it doesn't apply at all, delete the whole section — don't leave empty `_TBD_` sections as noise.
3. Fill each kept section concretely:
   - Define ambiguous or domain-specific terms inline where they first appear; fall back to the Glossary only when inline definitions get unwieldy (the template treats the Glossary as a last resort).
   - For schema or storage changes, cover the data model and migration concerns in the Data model / migration section: schema changes, backfill, rollback, and whether the change can ship without downtime.
   - Cover the API/interface surface in Interfaces: new or changed endpoints, naming, error handling, and backward compatibility.
   - Call out security, privacy, and access-control impact in the Security / Privacy sections when relevant.
4. The **Appendix** sections (Alternatives considered, Open issues) are not governed by the Optional delete rule in step 2 — keep them by default, since that's where design-review value concentrates, and drop one only if genuinely nothing applies. Record the trade-offs of the chosen approach alongside the rejected alternatives. (Resolved issues starts empty in a fresh draft — omit it until a decision actually moves there.)
5. If a tracker issue (Linear, etc.) is referenced, link it in the Metadata URL field. Never fabricate a URL; leave the field blank if unknown.

## Output

- Produce a single Markdown document. Write it in the language the user is working in (default to their language) — translate the section headings into that language too rather than leaving them in English.
- Don't state unresolved decisions as if they're settled. Put anything you couldn't decide into Open issues.
