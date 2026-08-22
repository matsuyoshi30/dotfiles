---
name: simplification-review
description: >
  Revisit an implementation, design, or diff and reshape it into the smallest
  safe form, free of over-engineering. Use when the user explicitly says
  simplification review, asks for a minimization review (最小化レビュー), or
  asks what code can be cut (削れるコードの検討) — optionally naming a target
  (a diff, a path, a design). Never run it unasked during ordinary
  implementation work: a pass that hunts for things to cut will always find
  some, so unrequested minimization strips code that was correctly sized and
  stalls work in second-guessing.
---

# Simplification Review

Understand the existing mechanism first, then choose the smallest change that meets the requirement.

Review the target the user named. With no target, review the working diff (`git diff HEAD`).

## Procedure

1. Confirm the goal, the constraints, and the existing implementation. Do not fill in missing premises by guessing.
2. Before adding an implementation, consider making it unnecessary, reusing an existing feature, changing configuration, or making a smaller change.
3. Compare options in this order.
   - Change nothing, or delete
   - Reuse an existing mechanism
   - Make a small, local change
   - Add a new abstraction, dependency, or feature
4. State the chosen option briefly, plus why the larger options were rejected.
5. Do not cut safety, authorization, input validation, failure behavior, or existing public contracts. Verify changes as needed.

## Criteria

- Do not add extensibility the requirement does not call for, abstractions for future use, or configuration options nobody reads.
- Factor out duplication only when several places genuinely share the same reason to change.
- Brevity is not the goal in itself. Prefer readability and keeping edits local.
- When uncertain, confirm with the smallest investigation or question before changing anything.
- "Already the smallest safe form" is a valid, complete result. Say so and stop — do not
  manufacture a cut to justify the review.

## Running the review in a fresh context

A review that inherits the context which produced the code also inherits the reasoning that
justified every extra layer, and defends it. When the target is work from the current
session and subagents are available, dispatch the review to a subagent that receives only
the requirement and the diff — never the reasoning behind it — then report its findings in
the format below. A reviewer that never saw the justification cannot be persuaded by it.

## Output

Lead with the conclusion — including "nothing to cut" when that is the conclusion — then
include only what is needed:

- What can be dropped
- The smallest change
- Safety and compatibility conditions that must hold
- Verification performed
