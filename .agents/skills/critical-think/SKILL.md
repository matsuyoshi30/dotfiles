---
name: critical-think
description: >
  Run a structured self-critique of Claude's own preceding response — hidden
  assumptions, logical gaps, overlooked risks, and AI-specific failure patterns
  (happy-path bias, over-engineering, hallucination, problem avoidance,
  context-blindness). Invoke only on an explicit user request such as
  "critically review that", "poke holes in your answer", "play devil's
  advocate", "critical think", or "what am I missing" — never automatically,
  and never merely because a topic seems complex or high-stakes: unrequested
  self-critique degrades helpfulness and contaminates later reasoning with
  manufactured doubt.
---

# Critical Think

A structured self-critique that Claude applies to **its own immediately preceding response**, in order to counter two known failure modes: the model's tendency toward confirmation bias (defending its own prior answer) and the user's tendency toward authority bias (accepting AI output too readily).

This is a deliberately **on-demand, opt-in** tool — not an always-on quality filter. Only run it when the user explicitly asks for it in this turn.

## When to use

Only on an explicit request in this turn ("poke holes in that", "before I commit, stress-test it", and the like — see the description for more trigger phrases). Good candidates for the user to invoke it on: architecture/design proposals, "this will definitely work" claims, large refactors, security- or performance-sensitive recommendations, or any response they are about to act on with real consequences. Never self-invoke for routine responses or just because a request seems important — the cost (tokens, time, risk of manufacturing false doubt) is only worth paying when asked.

## How to run it

### Resolving the target

- Default target: Claude's own response immediately prior to this request — not the user's message.
- If the invocation itself supplies user-authored content to review (a plan, document, or code pasted alongside the trigger), treat that as an explicit redirect: open the critique by stating that the target is the user's content, then apply the sections below with the AI-specific wording read accordingly — e.g. Initial Confidence rates the confidence the document itself projects, Hallucination becomes "claims or figures that should be verified", and Context Ignorance is skipped when there is no prior conversation to contradict.
- If there is no preceding Claude response and no clearly supplied target — including when it's ambiguous whether attached content was meant as the target — say so and ask what to critique; never reconstruct or invent a prior answer.

Produce the critique using the five sections below, written in the language of the conversation under review (section labels may stay in English). Keep judgments concrete and specific to the actual content being reviewed — avoid generic, interchangeable boilerplate that could apply to any answer.

### 1. Core Thesis & Confidence
- **Core Thesis**: State the single main claim or recommendation of the prior response, in one sentence.
- **Initial Confidence**: Rate 1–10 the confidence the original response itself projected, implicitly or explicitly (not your confidence in this critique), and whether that confidence was earned.

### 2. Foundational Analysis: Assumptions & Context
- **High-Impact Assumptions**: List the assumptions the response depended on that were never verified or stated (e.g., about the user's environment, scale, constraints, or intent). Focus on assumptions that would change the recommendation if wrong.
- **Contextual Integrity**: Note any place the response resolved ambiguity in the user's request silently, without flagging that a judgment call was made.

### 3. AI-Specific Pitfall Analysis
Evaluate each as **Pass** or **Fail**, with a one-line reason. Skip a dimension only when it doesn't apply to this content at all (e.g. Context Ignorance when there was no prior context or stated constraint to honor) rather than forcing a verdict; a dimension that applies and was handled acceptably is a Pass, and a minor gap is better noted as a brief caveat on a Pass than escalated to a Fail. For Hallucination, unverified figures or predictions stated as fact count as a Fail — outright fabrication is not required. When tools are available, verify checkable claims directly (read the referenced file, check the API, run the command) and cite the result, instead of only flagging them as needing verification:
- **Problem Avoidance**: Did it substitute mocks, placeholders, or hand-waving for the actual hard part of the problem?
- **Happy-Path Bias**: Did it ignore error handling, edge cases, or failure modes?
- **Over-Engineering**: Did it reach for more complexity, dependencies, or abstraction than the problem needed?
- **Hallucination**: Did it reference APIs, functions, facts, sources, or figures that should be verified rather than trusted?
- **Context Ignorance**: Did it drop or contradict constraints established earlier in the conversation, including ones stated in the message it was answering?

### 4. Overlooked Risks & Alternatives
- **Overlooked Risks**: Concrete risks, failure scenarios, or costs the original response didn't mention.
- **Alternative Scenarios**: At least one genuinely different approach (not a minor variant) that was available and not discussed, with a brief note on its trade-off.

### 5. Synthesis & Revised Recommendation
- **Net Assessment**: Does the critique change the recommendation, or does the original hold up? Say so plainly — don't manufacture doubt where the original response was actually sound.
- **Actionable Next Step**: One concrete next step for the user — e.g., a clarifying question to resolve, a specific thing to verify, or a revised recommendation.

### Running the critique in a fresh context

Self-critique inherits the biases of the context that produced the answer. When the user is about to act with real consequences and subagents are available, prefer dispatching the critique to a fresh subagent that receives only the user's request and the response under review — not the reasoning behind it — then present its findings in the five sections above. A reviewer that never saw the reasoning can't defend it.

## After presenting the critique

Offer the user a lightweight way to avoid this critique distorting later turns, since a self-critique sitting in the conversation history can bias subsequent responses toward manufactured caution (context contamination). For example: "If any of this feels off-base, say so and I'll set it aside rather than let it steer what comes next." Don't silently treat the critique's concerns as settled facts in later responses — they are hypotheses for the user to weigh, not conclusions.

## Guardrails

- This is a tool for surfacing blind spots, not a verdict. The human makes the final call.
- Avoid reflexively finding fault in every section just to fill it out — if a section genuinely doesn't apply or the original response handled it well, say so briefly instead of inventing a criticism. A uniformly critical verdict is fine when each finding has specific grounds; the ban is on invented criticisms, not honest ones.
- Don't use this to critique the user's own ideas or messages — it applies to Claude's own preceding response only, unless the user explicitly redirects it elsewhere.
