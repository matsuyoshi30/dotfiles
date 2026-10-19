# Sources and Scope

Verified: 2026-10-19.

## Target book

Alistair Cockburn, Writing Effective Use Cases, published in Japanese as Use Case Jissen Guide: Koukateki na Use Case no Kakikata. Bibliographic details were checked against [Shoeisha's page for the 2001 edition](https://www.shoeisha.co.jp/book/detail/9784798101279). The complete book and Japanese translation were not cross-checked. Consistency of Japanese terminology across editions remains unverified.

## Method sources

Alistair Cockburn, [Structuring Use Cases with Goals](https://www.cs.otago.ac.nz/coursework/cosc461/usecases.htm), HaT Technical Report 1995.01, published in JOOP in 1997. This is the author's paper hosted by a university. Relevant sections: Problems 1–5.

Concepts adopted: use cases as collections of scenarios sharing a goal; coverage through goal achievement or abandonment and completion of responsibilities; primary and supporting actors; scope, goal level, and interaction detail as separate dimensions; summary, user-goal, and subfunction levels; interactions independent of screen operations; structuring through extensions and subordinate use cases.

Alistair Cockburn, [Basic Use Case Template](https://www.cs.otago.ac.nz/coursework/cosc461/uctempla.htm), TR.96.03a, version dated 1998-10-26. This is the author's template hosted by a university. Relevant sections: the opening note, “Using, Staging, Tailoring the Template,” and the template itself.

Concepts adopted: progressively detailing selected items from an inventory; preconditions, triggers, success states, main success scenarios, extensions, variations, and open issues.

The opening note says the book renamed Failed End Condition to Minimal Guarantee and Subvariation to Technology and Data Variations List. Do not present the older labels as the book's field names. Do not restrict the minimal guarantee to a state that holds only on failure.

The template is copyrighted by Humans and Technology. It explicitly permits copying and distribution with source attribution, and using and modifying the template in projects.

## Conventions added for this skill

The inventory columns with IDs, mapping to original requirements, distinction between assumptions and unresolved questions, question prioritization, findings format, and the rule for stating the primary actor when an intermediary operates the subject were added for practical use. Do not attribute these conventions to the author's wording or present them as mandatory book procedures.

This skill handles use case organization and analysis separately from broader requirements discovery and specification workflows. It does not require evidence ledgers, implementation plans, or code generation as universal prerequisites.
