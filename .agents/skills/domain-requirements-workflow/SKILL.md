---
name: domain-requirements-workflow
description: Domain understanding and requirements-discovery workflow with evidence traceability, hypothesis management, persona-based interviews, sufficiency scoring, and requirements document generation. Use when Codex needs to clarify ambiguous product or business requirements, deepen understanding of a business domain, preserve source-backed findings during research, interview generated personas or stakeholders, or turn scattered notes into traceable requirements and requirement documents.
---

# Domain Requirements Workflow

## Overview

Drive a requirements-discovery run from ambiguous inputs to a traceable requirements document. Preserve every important claim back to a recorded source and evidence fragment.

Read [references/source-traceability.md](references/source-traceability.md) before collecting any information. Read [references/workflow-spec.md](references/workflow-spec.md) before orchestrating a run. Read [references/prompts-and-state.md](references/prompts-and-state.md) when role prompts or state layout are needed.

## Run Setup

Create a run directory before starting substantive work.

Use this file layout unless the surrounding repository already has a better convention:

```text
work/requirements-run/
  state.json
  source-ledger.json
  evidence-log.json
  claim-traceability.json
  drafts/
  interview-notes/
  final-requirements.md
```

Initialize these artifacts immediately:

- `state.json`
- `source-ledger.json`
- `evidence-log.json`
- `claim-traceability.json`

Do not wait until document generation to capture sources.

## Operating Rules

- Fix the question, scope, out-of-scope items, and success criteria before generating personas.
- Record every referenced document, meeting, interview, or observation in the source ledger at first use.
- Split observations into evidence fragments. Keep each fragment to one meaning unit.
- Classify each fragment as fact, inference, or request.
- Refuse to promote unsupported statements into accepted needs or requirements.
- Keep conflicting evidence visible. Do not silently merge it away.
- Separate needs from requirements.
  - A need states why something matters.
  - A requirement states what must be true or testable.
- Treat persona outputs as structured hypotheses until evidence strengthens them.

## Workflow

### 1. Fix the decision question

Write a one-sentence question for the decision this run must support. Capture:

- in-scope work
- out-of-scope work
- intended readers
- success criteria

If any of these are missing, stop and narrow the task before exploring further.

### 2. Build the first domain map

Extract:

- actors
- events and process flow
- inputs and outputs
- terms
- candidate constraints
- unknowns

Use the domain map to expose what is unclear, not to pretend the model already knows the domain.

### 3. Record hypotheses

List open hypotheses for:

- user pain
- operational constraints
- policy or approval constraints
- priority drivers

Attach a validation plan to each hypothesis.

### 4. Plan information collection

Decide which questions need:

- documents
- interviews
- observation
- follow-up later

Prefer the cheapest credible source first.

### 5. Generate role-based personas

Generate a small set of personas that reflect role differences, not personality fiction. Start with three to five perspectives unless the domain clearly requires more.

For each persona, capture:

- role
- responsibilities
- success conditions
- constraints
- context
- target hypotheses
- confidence level

### 6. Run interviews and extract evidence

Ask for:

- normal flow
- exception flow
- failure handling
- approvals
- timing and frequency
- handoffs
- legal or policy constraints

After each answer:

1. register the conversation source if needed
2. extract evidence fragments
3. update hypotheses, unknowns, and conflicts

### 7. Score sufficiency

Score each major topic on:

- importance
- evidence strength
- coverage of exception cases
- implementation or operational impact

Use the score to decide whether to continue exploring or move into requirement shaping.

### 8. Re-explore only the gaps

Investigate only the unresolved or weakly supported topics. If repeated probing produces only abstract answers, narrow the question or mark the topic undecided instead of inventing certainty.

### 9. Shape needs

Write needs as stakeholder-centered statements with:

- identifier
- statement
- background
- priority
- confidence
- supporting evidence identifiers

Do not describe screens or solutions at this stage unless the need itself depends on them.

### 10. Derive requirements

Translate needs into testable requirements with:

- identifier
- linked need identifiers
- requirement type
- statement
- acceptance conditions
- constraints
- supporting evidence identifiers

Reject requirements that cannot be traced to needs and evidence.

### 11. Generate the final document

Include at minimum:

- decision question
- scope and out-of-scope items
- key domain findings
- needs
- requirements
- undecided items
- source list
- claim-to-evidence mapping

### 12. Validate before claiming completion

Check:

- Does the output answer the original decision question?
- Does every major need have evidence?
- Does every major requirement trace to needs and evidence?
- Are unresolved conflicts and gaps explicit?

If any answer is no, go back to the relevant stage.

## Reference Loading Guide

- Read [references/source-traceability.md](references/source-traceability.md) before any research or interviews.
- Read [references/workflow-spec.md](references/workflow-spec.md) when stage transitions, completion checks, or return conditions matter.
- Read [references/prompts-and-state.md](references/prompts-and-state.md) when implementing orchestration, role prompts, or file schemas.

## Deliverable Standard

Prefer a concise final requirements document plus machine-readable ledgers. Keep the narrative short and let the traceability files carry the audit trail.
