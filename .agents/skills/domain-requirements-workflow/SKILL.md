---
name: domain-requirements-workflow
description: Use when clarifying ambiguous product or business requirements, deepening understanding of a business domain, researching an unfamiliar domain from scratch when no reference materials have been provided yet, preserving source-backed findings during research, probing a domain through generated personas or real stakeholders, or turning scattered notes into traceable, audit-ready requirements. For heavyweight domain research that needs an audit trail; for lightweight idea shaping use brainstorming or shaping-spec instead.
---

# Domain Requirements Workflow

## Overview

Drive a requirements-discovery run from ambiguous inputs to a traceable requirements document. Preserve every important claim back to a recorded source and evidence fragment.

This file is enough to run the workflow. Load a reference only when its depth is needed — see the Reference Loading Guide at the end. The one hard prerequisite: read [references/source-traceability.md](references/source-traceability.md) before collecting any information.

## When to use this and how much process

Default to a lightweight run. Escalate to the full 12-stage run with JSON ledgers only when at least one of these holds:

- the requirements will be formally audited or defended later
- evidence conflicts that must be tracked and resolved separately
- so many sources and claims that the trail no longer fits in a single readable markdown file

Stakeholder count alone is not a trigger — a domain with many roles but one source and no conflicts still fits a lightweight run.

Lightweight run: skip the JSON ledgers and run the spine in markdown — decision question, what real sources said (with where it came from), needs, requirements, undecided items, human confirmation. Add ledgers only when one of the escalation triggers appears.

When not to use: for shaping a rough idea or stress-testing one design, use brainstorming, shaping-spec, or grill-me — they are faster and do not impose source traceability.

## Run Setup

Create a run directory before starting substantive work. Put it in the target project's work area, or in a scratchpad directory if one is available. Never create it inside a tool's or skill's own source tree — that pollutes the source. When in doubt, use a scratchpad.

Use this file layout unless the surrounding repository already has a better convention:

```text
work/requirements-run/
  state.json
  source-ledger.json
  evidence-log.json
  drafts/
  interview-notes/
  final-requirements.md
```

For a full run, initialize these artifacts immediately:

- `state.json`
- `source-ledger.json`
- `evidence-log.json`

For a lightweight run, skip the JSON files and keep the same information inline in the markdown spine — a source list and evidence notes with their locators. The rule is capture sources as you go, whichever form you use; do not wait until document generation to capture them.

Traceability lives inside the need and requirement records (their `supported_by` and `need_ids` fields) in `state.json`. The claim-to-evidence mapping in the final document is generated from those records — keep it as a view, not a fourth file to hand-sync.

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
- Evidence enters only from real sources: real documents, real stakeholder conversations, and direct observation. Each must have a real `source_id` in the source ledger.
- Model-generated persona statements are never evidence. They are hypotheses and questions to verify against real sources. Never write them to the evidence log.
- You may gather real sources yourself by searching the web or fetching documents (with whatever search/fetch tool your runtime provides) — you do not have to wait for the user to hand them over. But the web is a real source only for general-domain facts, never for this organization's specific operations or final requirements. A web finding is a hypothesis by default; admit it as general-domain evidence only when it comes from an authoritative named source (a standards body, regulation, formal specification, or primary documentation). Record the search query and access date with the URL so a human can audit what was searched and what was left out, and verify the URL resolves before logging it — never cite a URL you have not confirmed exists (see [references/source-traceability.md](references/source-traceability.md)).
- Confirm the final needs and requirements with a real stakeholder (or the user) before claiming completion. Self-scoring is not validation, and no web-derived claim becomes a confirmed organization-specific requirement without that confirmation.

## Red flags — stop and recheck

These are the rationalizations that break a run. If you catch yourself in one, stop.

| Rationalization | Reality |
|---|---|
| "This persona is basically a domain expert, so its answer can count as evidence" | Personas are model-generated. Their statements are hypotheses and questions, never evidence. Never write them to the evidence log. |
| "This topic is high-importance, so it's fine to call it ready on thin evidence" | Importance is never added to readiness. Evidence strength 0 means re-explore, whatever the importance. |
| "A real source says the spec is unknown — that's evidence, so I'll raise the strength" | Evidence of a gap is not evidence of the topic. That topic's evidence strength stays 0. |
| "Self-scoring all passed, so the run is complete" | Self-scoring checks internal consistency, not wrong assumptions. Not complete until a real stakeholder (or the user) confirms it. |
| "It's mostly hypotheses, but I'll package them as requirements anyway" | A topic with no real-source evidence stays draft. It is not promoted to a confirmed requirement. |
| "The two sources disagree — I'll pick the better one and move on" | Conflicts are not flattened. Record the conflict and leave its resolution undecided. |
| "I searched, found a source that agrees, and logged it as evidence" | You chose the query, the result, and the snippet — that launders a prior. Web is a hypothesis unless it comes from an authoritative named source. Record the query and date so the selection can be audited. |
| "A web standard says X, so this organization does X" | General-domain facts are not org-specific facts. The web never establishes this org's operations or a confirmed requirement — that needs a real stakeholder. |
| "This URL looks right, so I'll cite it as the source" | A plausible URL is not a verified one — models hallucinate URLs. Fetch it and confirm it serves the cited content before logging it. If it does not resolve, do not cite it. |

## Workflow

Stages 1-4 (setup) and 8-11 (shaping) are mechanical — one line each below; their outputs, completion checks, and return conditions live in [references/workflow-spec.md](references/workflow-spec.md). The discipline that makes or breaks a run is in stages 5-7 and 12, plus the Operating Rules and Red flags above.

### Setup (stages 1-4)

1. Fix the decision question — one sentence, plus in-scope, out-of-scope, intended readers, success criteria. Missing any → stop and narrow before exploring.
2. Build the first domain map — actors, event flow, inputs/outputs, terms, candidate constraints, unknowns. Use it to expose what is unclear, not to fake knowledge.
3. Record hypotheses — user pain, operational and policy constraints, priority drivers — each with a validation plan.
4. Plan information collection — choose a source per open question (document, interview, observation, authoritative web/literature for general-domain background, later follow-up); prefer the cheapest credible source first.

### 5. Generate role-based personas

Generate three to five personas that reflect role differences, not personality fiction (more only if the domain demands it). Capture for each: role, responsibilities, success conditions, constraints, context, target hypotheses, confidence level.

Personas are a source of questions and hypotheses, never a source of evidence (see Red flags). Use them to expose what to ask and where the risky unknowns are, not to manufacture answers. Personas cover org-role perspectives; for general-domain background expertise (industry standards, regulations, common practice), dispatch the `domain-expert-agent` instead — see stage 6.

### 6. Collect from real sources, then extract evidence

Probe the following for every flow:

- normal flow
- exception flow
- failure handling
- approvals
- timing and frequency
- handoffs
- legal or policy constraints

There are distinct modes. Keep them separate:

- Simulated persona dry-run: have a persona answer to surface candidate flows, expected pain, and the exact questions a real source must confirm. Record these as hypotheses and open questions. Never write them to the evidence log.
- Real source (real document, real stakeholder conversation, or observation): the only thing that produces evidence. For each, register the source at first use, extract evidence fragments, and classify each as fact, inference, or request.
- Web and literature search: gather real sources for general-domain background yourself instead of waiting for the user. Register each with its URL, the search query, and the access date, and verify the URL resolves before logging it (drop or re-find any that 404 or are unreachable); rate reliability by the source's authority. Treat a finding as a hypothesis unless it comes from an authoritative named source, and never let it stand in for this organization's specific operations — those still need a real stakeholder. You may delegate this general-domain research to the `domain-expert-agent` (it writes a briefing to a path you give); its output enters as hypotheses, promotable to general-domain evidence only when grounded in an authoritative named source, and never as org-specific evidence.

After each real-source answer:

1. register the source if needed
2. extract evidence fragments
3. update hypotheses, unknowns, and conflicts

If no real source has answered a topic yet, that topic stays at hypothesis strength — do not score it as covered. When neither a stakeholder nor an internal document is available, an authoritative web or literature source can still establish general-domain facts (moving a general topic off evidence strength 0), but org-specific topics stay undecided until a real stakeholder confirms them.

### 7. Score sufficiency

Score readiness separately from importance — importance never makes a topic better understood, so never add it to the readiness total.

- Readiness = evidence strength (0-2) + exception coverage (0-2).
- Rate importance and operational impact as low / medium / high to set how high the bar is.

Decide:

- Evidence strength 0 (only simulated or assumed) means re-explore, whatever the importance.
- The bar gates which claims you may shape, not whether you touch the topic. From a high-importance topic you may still shape a requirement that rests only on confirmed facts, and defer the parts that depend on weak or missing evidence as undecided. Never shape a requirement whose own support is below the bar.
- Treat a high-importance or high-impact topic as fully resolved only at readiness 3 or more; readiness 2 may pass for lower importance.

A real source that only confirms a topic is unknown or unresolved is evidence of a gap, not of the topic — it does not raise that topic's evidence strength. An inference written in a real source is still evidence (classify it `inference`); only the model's own guesses are barred.

See [references/workflow-spec.md](references/workflow-spec.md) for the per-axis rubric.

### Shaping (stages 8-11)

8. Re-explore only the gaps — investigate only unresolved or weakly supported topics. If probing yields only abstract answers, narrow the question or mark the topic undecided rather than invent certainty.
9. Shape needs — each a stakeholder-centered, why-level statement with id, statement, background, priority, confidence, and supporting evidence ids. No screens or solutions unless the need depends on them.
10. Derive requirements — testable, with id, linked need ids, type, statement, acceptance conditions, constraints, and supporting evidence ids. Reject any that cannot be traced to needs and evidence.
11. Generate the final document — decision question, scope and out-of-scope items, key domain findings, needs, requirements, undecided items, source list, and claim-to-evidence mapping. In the source list, show each web source's verified URL (state plainly when none was captured); render the evidence fragments and the claim-to-evidence mapping as markdown tables. See the output format in [references/workflow-spec.md](references/workflow-spec.md).

### 12. Validate before claiming completion

Run a self-check first:

- Does the output answer the original decision question?
- Does every major need have evidence from a real source?
- Does every major requirement trace to needs and evidence?
- Are unresolved conflicts and gaps explicit?

If any answer is no, go back to the relevant stage.

Then confirm with a human. Self-scoring catches internal inconsistency, not wrong assumptions. Before claiming completion, present the major needs, requirements, and undecided items to a real stakeholder (or the user) and ask whether they match reality. Run this check in a fresh context where practical (see the Critic role in [references/prompts-and-state.md](references/prompts-and-state.md)) so the reviewer is not the author. Record disagreements as new evidence or open conflicts and revisit the relevant stage.

## Reference Loading Guide

- Read [references/source-traceability.md](references/source-traceability.md) before any research or interviews.
- Read [references/workflow-spec.md](references/workflow-spec.md) when stage transitions, completion checks, or return conditions matter.
- Read [references/prompts-and-state.md](references/prompts-and-state.md) when implementing orchestration, role prompts, or file schemas.

## Deliverable Standard

Prefer a concise final requirements document plus machine-readable ledgers. Keep the narrative short and let the traceability files carry the audit trail.
