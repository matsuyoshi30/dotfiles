---
name: lean-formal-audit
description: Audit and verify software systems in any domain — cryptographic protocols, ZK circuits, smart contracts, distributed systems, business logic (payments, inventory, access control), embedded systems, APIs, and more — by formalizing them in Lean 4. Use not only for auditing existing implementations but also for product architecture decisions (comparing design candidates, examining invariants, sanity-checking design soundness). Always use this skill when the user says things like "check / audit / prove it with Lean", "verify soundness or safety", "I want to formally confirm the design is correct", "verify or compare architecture candidates", "create an audit folder and formally verify", or when they ask to check consistency between implementation and design, invariants, or security properties. Apply the same full procedure to re-audit requests for new versions (v2, etc.). Always begin with an interview (questions from Claude to the user) to pin down the target, the mode, and the properties to verify — tailored to the case — before starting any work.
---

# Lean Formal Verification & Audit Skill (lean-formal-audit)

A workflow that formalizes a target system (in any domain) in Lean 4 and proves/verifies
soundness, safety, and design invariants. It has two modes:

- **Audit mode**: transcribe an existing implementation into Lean and attempt proofs to
  surface problems.
- **Architecture-decision mode**: before implementation (or during a redesign), formalize
  design candidates in Lean and compare which properties each candidate does or does not
  satisfy, to support product design decisions.

The essence is not that proofs "go through" but discovering **where and why they fail**
and **which assumptions were required**. This applies to both modes.

## Stage 0: Interview (always do this first)

Before starting any work, ask the user questions to pin down the target and scope. Do not
re-ask things already known from the conversation or uploaded files — only ask about the
items that are still open. If a tool that presents choices in the chat UI is available
(e.g. `AskUserQuestion` / `ask_user_input`), use it actively. Do not pile up too many
questions at once; ask 1–3 at a time, in stages.

Items to pin down:

1. **Mode**: audit of an existing implementation, architecture decision (verifying/comparing
   design candidates), or both.
2. **Components of the target**: which layers/components make up the system?
   Examples (rephrase to fit the domain):
   - Crypto/blockchain: ZK circuits, smart contracts, clients, relayers
   - Web products: domain logic, DB schema/transactions, API layer, frontend
   - Distributed systems: node state machines, consensus protocol, network assumptions
   - Embedded: control logic, hardware assumptions, fail-safes

   Based on the user's answer, replace the "component breakdown" of Stage 2 below with
   this component list.
3. **Location of inputs**: code paths/repositories, design documents, specifications,
   diagrams. In architecture-decision mode where no implementation exists yet, receive the
   design candidates (one or more) as prose, pseudocode, or diagrams.
4. **Properties to verify**: what must hold for the system to be "correct"? Examples:
   conservation of assets, absence of double processing, absence of privilege escalation,
   data consistency, idempotency, fault tolerance, replay resistance, liveness. If the
   user cannot articulate them, offer the standard properties of the domain as candidates
   and let them choose.
5. **Assumptions / threat model**: what is trusted and what is considered adversarial?
   Cryptographic assumptions, availability of external services, tolerance of single
   points of failure, etc. Assumptions fixed here are later managed centrally as `axiom`s.
6. **Out of scope**: code/components that need not be examined (UI, logging, experimental
   code, etc.).

Summarize the interview results in `audit/SCOPE.md`, get the user's confirmation, and only
then proceed to Stage 1. All subsequent stages use this SCOPE.md as their base context.

## Prerequisites (common rules)

- Create a new `audit/` folder at the working root and put all Lean artifacts there.
- Use Lean 4 (with Mathlib if possible). Even without a build environment, write in a
  form that would type-check.
- In audit mode, every Lean definition/lemma must carry a comment citing the corresponding
  implementation file name and line numbers. Example:
  `-- src/transfer.ts L42-L45: balance check`
  In architecture-decision mode, cite the corresponding design-document section, diagram,
  or candidate name instead.
- For re-audits of new versions (v2, etc.), run all stages again from the beginning. If
  old Lean code is reused, make the diff explicit and re-derive every line-number/reference
  comment.

## Workflow (4 stages)

Always proceed in this order, using each stage's artifacts as the base context for the
next stage.

### Stage 1: Lean formalization of the abstract design and the security/correctness model

In `audit/`, express the system's overall abstract design in Lean.

- Define the system state as structures (balances, inventory, sessions, replica states,
  commitments, etc. — whatever fits the domain).
- Define state transitions (operations, events, messages) as functions or inductive
  relations.
- State the "properties to verify" fixed in Stage 0 as explicit propositions.
- Declare the assumptions/threat model fixed in Stage 0 (hash collision resistance,
  partially synchronous network, response guarantees of external APIs, etc.) as explicit
  `axiom`s, with a comment saying "this is an assumption".
- In architecture-decision mode with multiple candidates, first define one common abstract
  state and property set, then express each candidate's differences (different transition
  rules) in separate namespaces.

Example artifacts: `audit/Abstract.lean` (plus per-candidate files such as
`audit/DesignA.lean`)

### Stage 2: Line-by-line / item-by-item Lean formalization per component

With Stage 1 as context, formalize each element of the component list fixed in Stage 0,
one by one.

**Audit mode (when an implementation exists):**

- Convert each component's implementation into Lean propositions/definitions line by line.
  Ideally, map one Lean proposition to each constraint, assertion, state update, or guard
  condition (require/if/throw).
- Cover only the parts essential to the soundness of other components; explicitly mark
  out-of-scope parts (fixed in Stage 0) as excluded and omit them.
- Do NOT "interpret" or "correct" while transcribing the implementation into Lean. Even if
  the implementation looks wrong, transcribe it as-is first and record doubts in
  `-- NOTE:` / `-- SUSPICION:` comments.
- Distinguish representation domains and arithmetic by type (e.g. finite field mod p vs
  uint256, floating point vs rationals, DB nullable vs non-nullable). Do not silently
  round to idealized numbers. Boundaries, overflow, and aliasing are primary sources
  of bugs.

**Architecture-decision mode (no implementation yet):**

- Formalize the design documents/pseudocode in Lean at the same granularity. Do not fill
  in behaviors the documents do not specify (on error, under concurrency, on retry) on
  your own; record them in `-- UNDERSPECIFIED:` comments and ask the user to pin them
  down. The list of "things not written down" is itself an important design-review
  deliverable.

Example artifacts: one file per component (`audit/Circuit.lean`, `audit/Contract.lean`,
`audit/Domain.lean`, `audit/ApiLayer.lean`, etc. — name them to match the Stage 0
breakdown)

### Stage 3: Soundness and safety proofs

Prove that the Stage 1 properties follow from the Stage 2 models.

- Rules of each component ⇒ the intended relations of the abstract design (consider both
  completeness and soundness directions).
- Each operation ⇒ preservation of state invariants (conservation laws, monotonicity,
  consistency, etc.).
- Composition of components ⇒ end-to-end properties.
- In architecture-decision mode, attempt proofs of the same property set for each
  candidate, and build a comparison table like: "candidate A satisfies property P without
  assumption X; candidate B requires assumption X".
- Where a proof does not go through, leaving `sorry` is allowed, but ALWAYS write a
  comment directly above it explaining **why it does not go through / what assumption
  would make it go through**. This is the raw data of the audit / design decision.
- Any `axiom` or strong hypothesis added for the sake of a proof must be appended to the
  Stage 1 assumption list and managed centrally.

Example artifacts: `audit/Soundness.lean`, `audit/Safety.lean` (plus
`audit/Comparison.md`)

### Stage 4: Full review per file / per candidate (question the assumptions)

Using all Lean artifacts from Stages 1–3, for each target file (or each design candidate),
one at a time:

1. Think hard about whether that file/candidate has soundness problems or concerns.
2. Re-read the corresponding Lean proofs and question the premises and assumptions.
   Checklist:
   - Any remaining `sorry`? If so, report it as a potential implementation/design defect.
   - Any sign of unreasonable assumptions introduced to force a proof through (unnaturally
     strong hypotheses, `axiom`s with no basis in the implementation/documents, implicitly
     added range constraints or ordering guarantees)?
   - Did the Lean side accidentally constrain values, ordering, or uniqueness that the
     implementation/design does not guarantee (missed under-constrained cases)?
   - Confusion of representation domains, missing range checks, edge cases such as zero
     values, empty sets, initial states, and concurrency.
   - Correspondence of interfaces between components (argument order, encodings, units,
     domain separation, idempotency keys, etc.).
3. Reach a conclusion per file/candidate: "no issues (with justification)" or "concern
   (content, severity, reproduction conditions)".

## Final report

Compile `audit/REPORT.md` with the following structure:

```
# Verification Report
## Target and version / Mode (audit or architecture decision)
## Scope (summary of the Stage 0 interview results)
## Method (summary of this skill's stages)
## Assumption list (axioms / unproven premises / UNDERSPECIFIED items)
## Findings per file / per candidate
## Findings (severity: Critical / High / Medium / Low / Info)
## List of sorry / unproven spots and their interpretation
## Conclusion (in architecture-decision mode: recommended option, selection rationale, trade-offs)
```

## Key mindset

- **Always start with questions.** Formalizing while the target, properties, and
  assumptions are still vague turns the proof into a verification of "a spec Claude made
  up on its own", which has no value.
- Always report **under which assumptions** something was proved, not merely that it
  "was proved".
- **Be faithful to the implementation/documents.** Intuitively "fixing" things while
  transcribing into Lean erases exactly the bugs and spec gaps you are supposed to detect.
- Reference comments (line numbers, document sections) are the backbone of traceability.
  When the target is updated (v2, etc.), re-derive all of them.
- Talk to the user in the vocabulary of their domain. Add short explanations of Lean
  jargon (axiom, sorry, inductive relations, etc.) as needed.
