---
name: analyzing-usecases
description: Organize and analyze use cases from business descriptions or requirements using Alistair Cockburn's public materials. Use for actor-goal discovery, scope and goal-level classification, success scenarios and extensions, or reviewing existing use cases. Do not use for UML diagramming alone or implementation design.
---

# Use Case Analysis

Organize responsibilities and success and failure paths around the goals users want to achieve. This workflow draws on public materials by the author of Writing Effective Use Cases, published in Japanese as Use Case Jissen Guide. It does not cover the entire book or its Japanese translation.

On first use, read [Sources and scope](references/sources.md). Inventory and output formats, and the handling of unresolved questions, are conventions added for this skill.

## Choose the task

- For scattered requirements or business descriptions, start with an actor-goal inventory. Do not detail every use case by default.
- When a specific goal is given, describe that use case. Do not delay the requested work to build a full inventory.
- When analyzing existing descriptions, identify problems and concrete corrections. Rewrite the whole description only when needed.

Distinguish information supported by the input, assumptions, and unresolved questions, using only these three markers. When you infer something the input does not state about an item it names, such as a step or a guarantee, mark that statement as an assumption where it appears, noting what it was inferred from. Do not invent business rules, permissions, retry counts, or compensation behavior. You may propose goals, actors, stakeholders, or failure conditions that the input does not name, including ones you inferred from it, but only in a separate table or section, never in the tables or lists of items the input names; mark them as assumptions and leave their rules and responses as questions. Continue with what can be established, and prioritize questions whose answers would change scope or success conditions.

## Organize scope and goals

State whether the subject is an entire organization, a system, or a component. Treat roles outside that boundary as actors. The same department or external service may be inside or outside depending on the chosen scope.

Identify the primary actor as the role seeking to achieve a goal with the subject's help, and supporting actors as parties whose help the subject needs. When one role holds the goal and another operates the subject on its behalf, such as a customer served by a clerk, state which one you treat as the primary actor and why, and record the other as a supporting actor or stakeholder; if the input does not settle how the work is performed, make that an unresolved question. When relevant, record stakeholders who do not interact directly and the interests that must be protected. Put actors and stakeholders that the input does not name in the separate proposal table, not in the actor or stakeholder list.

Separate the size of the scope from the goal level, using these criteria:

| Goal level | Classification guide |
|---|---|
| Summary | Groups multiple user goals and provides broader business context |
| User goal | Completes a meaningful piece of work for the primary actor |
| Subfunction | Performs part of the work needed to achieve another goal |

Use these inventory columns: `ID / Primary actor / Goal / Scope / Goal level / Related higher-level goal / Evidence or unresolved questions`. If the purpose or goal level cannot be established, mark it unresolved and explain what information is missing. Add priority when useful, but do not rank goals without a basis.

Do not turn screen names, button actions, or API names directly into goals. Ask “Why?” to find higher-level goals and “How?” to find subordinate work. Classify login and search in context. Do not multiply alternative means of achieving the same goal into separate user goals. Preserve the mapping to original requirements when merging or splitting items.

## Analyze scenarios

Name the selected goal with a short active verb phrase. Fill in only the fields needed, in successive passes.

- Preconditions: States already established before the use case starts, which the subject does not check again. A condition the subject must check belongs in a step, with its failure in an extension. When the input does not establish such a state, record it under Assumptions instead and leave it out of this field, which may then be empty.
- Trigger: The event that starts the use case.
- Success guarantee: The state that holds when the goal is achieved.
- Minimal guarantee: Interests or states protected even when the goal is not achieved. Mark the content unresolved when unknown; candidate guarantees may be listed as options to confirm.

Describe the main success scenario from the trigger through goal achievement and completion of the subject's responsibilities. Make the actor and action explicit in each step. During requirements analysis, avoid screen operations and implementation details; keep steps at a consistent level that explains information exchanges and responsibilities.

Consider failures, alternatives, and interruptions for each step, listing conditions the input does not mention separately from those it does. Record extensions as `Affected step / Branch condition / Response / Rejoin point or end state`. Extensions also include alternative paths to success. When the response is unknown, present options and questions rather than asserting behavior.

Put differences limited to media or data formats under “Technology and data variations.” When a difference changes behavior, describe it through extensions or subordinate use cases. When extracting a complex subordinate goal, preserve its success and failure consequences in the calling use case. Do not begin by choosing UML include / extend relationships.

## Present and review

Write in the user's language, translating field and column labels and keeping each translation consistent within the output. For organization tasks, present the goal inventory and reasons for merging or splitting items. For detailed descriptions, use the following minimum structure, adding stakeholders, supporting actors, and variations when needed. Mark assumptions where they appear, in addition to listing them.

```text
UC-ID: Goal
Scope / Goal level / Primary actor:
Preconditions / Trigger:
Success guarantee / Minimal guarantee:
Main success scenario: Numbered steps
Extensions: Affected step, condition, response, rejoin point or end state
Assumptions: Premises adopted to proceed without support in the input
Unresolved questions: Missing evidence and decisions needed
```

Present findings as “Location → Problem and impact → Correction or question.” Look especially for mixed scopes, mixed goal levels, paths that do not establish the success guarantee, and responsibilities omitted on failure. Distinguish “organized” from “confirmed”: the input alone cannot establish completeness or stakeholder agreement. Do not proceed to implementation planning or code generation unless requested.
