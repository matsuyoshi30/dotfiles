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
Create role-based viewpoints to probe the domain.

Outputs:
- personas
- persona-to-hypothesis links

Completion checks:
- Personas represent distinct roles.
- Every persona helps validate at least one hypothesis.

Return conditions:
- Persona details are decorative rather than operational.

## 6. Interviews and evidence extraction

Goal:
Collect normal flow, exception flow, and operational reality.

Outputs:
- interview notes
- evidence fragments
- new unknowns
- conflicts

Completion checks:
- Key hypotheses received concrete examples.
- Exception paths were discussed.

Return conditions:
- Answers stay abstract.
- No evidence fragments were extracted.

## 7. Information evaluation

Goal:
Judge whether the current evidence is enough to shape needs and requirements.

Outputs:
- sufficiency scores
- gaps
- conflicts
- claim-to-evidence mapping

Score each major topic from 0 to 2 on:

- importance
- evidence strength
- exception coverage
- implementation or operational impact

Interpretation:

- 6 or more: acceptable for now
- 4 to 5: likely needs follow-up
- 3 or less: re-explore

Return conditions:
- A major claim lacks evidence.
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

Completion checks:
- Confirmed items and undecided items are clearly separated.

Return conditions:
- Traceability information is missing from the output package.

## 12. Validity review

Goal:
Check whether the final package actually supports the original decision.

Outputs:
- approval to finish
- or stage to revisit

Completion checks:
- The original decision question is answered.
- Important claims and requirements are traceable.
- Conflicts and gaps are visible.
