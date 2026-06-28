# Prompts And State

## Purpose

Use this reference when implementing the skill as a multi-role workflow or when preparing the working files for a requirements-discovery run.

## Recommended Working Files

Maintain these files throughout the run:

1. `state.json`
2. `source-ledger.json`
3. `evidence-log.json`
4. `claim-traceability.json`
5. `final-requirements.md`

## State Shape

```json
{
  "meta": {
    "run_id": "req-20260628-01",
    "created_at": "2026-06-28T13:30:00+09:00",
    "updated_at": "2026-06-28T13:45:00+09:00",
    "current_stage": "information_evaluation",
    "status": "active"
  },
  "goal": {
    "question": "Which decision this run supports",
    "in_scope": [],
    "out_of_scope": [],
    "success_criteria": [],
    "audiences": []
  },
  "domain": {
    "actors": [],
    "events": [],
    "inputs_outputs": [],
    "terms": [],
    "constraints": [],
    "unknowns": []
  },
  "hypotheses": [],
  "personas": [],
  "requirements": {
    "needs": [],
    "system_requirements": [],
    "undecided": [],
    "decisions": []
  },
  "quality": {
    "coverage_summary": [],
    "conflicts": [],
    "gaps": [],
    "readiness": "not_ready"
  }
}
```

## Role Prompts

### Orchestrator

```text
Act as the workflow orchestrator.
Keep the run aligned to the decision question.
Do not allow stage transitions until completion checks are met.
Refuse to accept major needs or requirements with no supporting evidence.
Keep gaps, conflicts, and undecided items visible.

Output:
- current stage
- pass or fail
- next role
- return reason if any
- state fields to update
```

### Researcher

```text
Act as the researcher.
Read materials, conduct structured interviews, update the domain map, and extract evidence.
Register every source at first use.
Record observations rather than conclusions whenever possible.
Classify each evidence fragment as fact, inference, or request.

Output:
- new sources
- new evidence fragments
- domain updates
- new unknowns
- new conflicts
```

### Critic

```text
Act as the critic.
Inspect the run for unsupported claims, weak evidence, terminology drift, and conflicts.
Score sufficiency and say whether the run can proceed.

Output:
- unsupported claims
- weak evidence areas
- conflicts
- re-exploration targets
```

### Editor

```text
Act as the editor.
Shape needs and requirements while preserving traceability.
Separate confirmed items from undecided items.
Keep the final document concise and auditable.

Output:
- need list
- requirement list
- undecided list
- claim-to-evidence mapping
```

## Need Record Shape

```json
{
  "need_id": "N-001",
  "statement": "Reduce delay in exception handling",
  "stakeholders": [
    "operations staff"
  ],
  "background": "Exception approvals create visible waiting time.",
  "priority": "high",
  "confidence": "medium",
  "supported_by": [
    "E-001",
    "E-007"
  ]
}
```

## Requirement Record Shape

```json
{
  "requirement_id": "R-003",
  "need_ids": [
    "N-001"
  ],
  "type": "functional",
  "statement": "Show exception status and blocking approver in one place.",
  "acceptance_conditions": [
    "Operations staff can identify the current approval step without opening another system."
  ],
  "constraints": [],
  "supported_by": [
    "E-001",
    "E-010"
  ]
}
```
