# Workflow Specification

## Purpose

Use this reference when the run needs explicit stage contracts, completion checks, or return conditions.

## Stages

1. Purpose confirmation
2. Initial domain map
3. Hypothesis list
4. Information collection plan
5. Persona generation
6. Interviews and evidence extraction
7. Information evaluation
8. Gap re-exploration
9. Need shaping
10. Requirement derivation
11. Document generation
12. Validity review

## 1. Purpose confirmation

Goal:
Fix the decision question and the boundaries of the run.

Inputs:
- request
- background material

Outputs:
- question
- in-scope items
- out-of-scope items
- intended readers
- success criteria

Completion checks:
- The decision question is one sentence.
- Out-of-scope items are explicit.

Return conditions:
- The task is too broad.
- The reader or approver is unknown.

## 2. Initial domain map

Goal:
Show the known business structure and expose what is unclear.

Outputs:
- actors
- event flow
- inputs and outputs
- terms
- candidate constraints
- unknowns

Completion checks:
- Start and end points of the business flow are explainable.
- Important terms are listed, even if definitions are incomplete.

Return conditions:
- The flow has no clear start or end.
- Terminology conflicts remain invisible.

## 3. Hypothesis list

Goal:
Separate assumptions from known facts.

Outputs:
- hypotheses
- validation plan per hypothesis

Completion checks:
- Each major unknown has a validation path.

Return conditions:
- A hypothesis has no way to validate it.
- Facts and hypotheses are mixed together.

## 4. Information collection plan

Goal:
Choose where each answer should come from.

Outputs:
- prioritized questions
- source strategy per question

Completion checks:
- Each major hypothesis has an acquisition path.

Return conditions:
- Questions are too broad to compare answers.
- Interviews are being used where documents would suffice.

## 5. Persona generation

Goal:
Create role-based viewpoints to probe the domain. Personas generate questions and hypotheses, not evidence.

Outputs:
- personas
- persona-to-hypothesis links

Completion checks:
- Personas represent distinct roles.
- Every persona helps validate at least one hypothesis.

Return conditions:
- Persona details are decorative rather than operational.
- Persona output is being treated as evidence rather than hypotheses.

## 6. Interviews and evidence extraction

Goal:
Collect normal flow, exception flow, and operational reality.

Keep two modes separate. A simulated persona dry-run produces hypotheses and the questions a real source must answer. Only a real source (real document, real stakeholder, or observation) produces evidence fragments.

Outputs:
- interview notes
- evidence fragments (from real sources only)
- new unknowns
- conflicts

Completion checks:
- Key hypotheses received concrete examples from a real source.
- Exception paths were discussed.

Return conditions:
- Answers stay abstract.
- No evidence fragments were extracted from real sources.
- A simulated persona answer was logged as evidence.
- A web source was logged without verifying its URL resolves.

## 7. Information evaluation

Goal:
Judge whether the current evidence is enough to shape needs and requirements.

Outputs:
- sufficiency scores
- gaps
- conflicts
- claim-to-evidence mapping

Readiness and importance are different things. Importance does not make a topic better understood. Score them separately and never add importance into the readiness total.

Score readiness on two axes, 0 to 2 each:

- evidence strength
  - 0: only simulated persona output or assumption
  - 1: one real source, or weak/indirect evidence
  - 2: multiple corroborating real sources
  - A real source that only confirms the topic is unknown or unresolved scores 0 for that topic — it is evidence of a gap, not of the topic. An inference recorded inside a real source still counts (classify it `inference`); only the model's own guesses score 0.
- exception coverage
  - 0: exceptions not explored
  - 1: main exceptions named but unconfirmed
  - 2: exceptions confirmed by a real source

Readiness = evidence strength + exception coverage (max 4).

Rate importance and operational impact as low / medium / high. These set how high the bar is; they are not added to readiness.

Decision:

- Evidence strength 0 means re-explore, regardless of importance. No real evidence is never "acceptable".
- The bar gates which claims may be shaped, not whether the topic is touched. From a high-importance topic, shaping a requirement that rests only on confirmed facts is allowed; defer the parts resting on weak or missing evidence as undecided. Never shape a requirement whose own support is below the bar.
- Treat a high importance or high impact topic as fully resolved only at readiness 3 or more; for lower importance, readiness of 2 may be acceptable. Record the rest as undecided rather than inventing certainty.

Return conditions:
- A major claim lacks real-source evidence.
- Importance was used to mark a low-evidence topic as ready.
- Conflicting evidence was flattened instead of recorded.

## 8. Gap re-exploration

Goal:
Probe only unresolved or weakly supported topics.

Outputs:
- additional evidence
- updated scores

Completion checks:
- Remaining gaps are either resolved or explicitly deferred.

Return conditions:
- The same abstract answer repeats.
- The top-level question itself needs narrowing.

## 9. Need shaping

Goal:
Describe stakeholder needs without collapsing into solution design too early.

Outputs:
- need identifiers
- statements
- priority
- confidence
- supporting evidence

Completion checks:
- Needs are written as why-level statements.
- Each important need has supporting evidence.

Return conditions:
- The statements are really feature ideas.
- The stakeholder is unclear.

## 10. Requirement derivation

Goal:
Convert needs into testable requirements.

Outputs:
- requirement identifiers
- linked need identifiers
- requirement type
- statement
- acceptance conditions
- constraints
- supporting evidence

Completion checks:
- Requirements are testable.
- Traceability to needs and evidence exists.

Return conditions:
- Acceptance conditions are missing.
- A requirement cannot be traced upward.

## 11. Document generation

Goal:
Produce a decision-ready requirements document.

Outputs:
- final requirements document
- source list
- claim-to-evidence mapping
- undecided items

Output format:
- Source list: show each web source's verified URL. When no URL was captured (e.g. a portal with no single page), say so plainly rather than omitting the field.
- Evidence fragments: render as a markdown table with columns id, source (with locator), summary, classification, confidence.
- Claim-to-evidence mapping: render as a markdown table with columns claim, evidence ids, source ids.

Completion checks:
- Confirmed items and undecided items are clearly separated.
- Every web source in the source list carries its verified URL, or an explicit note that none was captured.

Return conditions:
- Traceability information is missing from the output package.
- A web source is listed without its URL or a note that none was captured.

## 12. Validity review

Goal:
Check whether the final package actually supports the original decision, and confirm it with a human.

Outputs:
- self-check result
- human confirmation result
- approval to finish, or stage to revisit

Completion checks:
- The original decision question is answered.
- Important claims and requirements are traceable to real-source evidence.
- Conflicts and gaps are visible.
- A real stakeholder (or the user) confirmed the major needs and requirements match reality.

Return conditions:
- The human flags a need or requirement as wrong or missing.
- A major claim traces only to simulated persona output.
