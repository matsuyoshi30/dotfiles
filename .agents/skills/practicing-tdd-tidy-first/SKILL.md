---
name: practicing-tdd-tidy-first
description: Guides implementation with Kent Beck's TDD cycle (Red-Green-Refactor) and Tidy First discipline (separating structural from behavioral changes). Use when writing or modifying production code, especially when driving implementation one failing test at a time or when a plan.md with a checklist of tests exists in the project.
model: sonnet
user-invocable: true
---

# Practicing TDD with Tidy First

Follow Kent Beck's TDD and Tidy First methodology precisely.

## Plan-driven loop (when plan.md exists)

If the project has a `plan.md` with a checklist of tests, treat it as the source of truth. When the user says "go":

1. Find the next unmarked test in `plan.md`
2. Implement that test (Red)
3. Write only enough production code to make it pass (Green)
4. Refactor if needed, keeping tests green
5. Mark the test done in `plan.md`

If no `plan.md` exists, skip this section and use the general TDD cycle below.

## TDD cycle

- **Red**: Write the simplest failing test that defines a small increment of behavior. Use descriptive test names (e.g., `shouldSumTwoPositiveNumbers`).
- **Green**: Write the minimum code to make the test pass — no more.
- **Refactor**: Only after green. Remove duplication, clarify intent. Run tests after each change.

For defects: first write an API-level failing test that reproduces the bug, then the smallest possible unit test that isolates it, then make both pass.

## Tidy First: separate structural from behavioral changes

Every change is exactly one of:

1. **Structural** — rearranging code without changing behavior (rename, extract, move). Verify tests pass before and after.
2. **Behavioral** — adding or modifying functionality.

Rules:
- Never mix structural and behavioral changes in the same commit.
- When both are needed, do structural first.
- Commit messages must state which type the commit contains.

## Commit discipline

Commit only when ALL of:
- All tests pass
- All compiler/linter warnings resolved
- The change is a single logical unit
- The message identifies the change as structural or behavioral

Prefer small, frequent commits.

## Quality bar

- Eliminate duplication ruthlessly
- Make dependencies explicit
- Keep methods small and single-purpose
- Minimize state and side effects
- Use the simplest solution that could possibly work

## Workflow checklist for a new feature

```
- [ ] Write one failing test for the smallest increment
- [ ] Make it pass with minimum code (Green)
- [ ] Run all tests (except long-running)
- [ ] Tidy First: any structural changes needed? Commit separately.
- [ ] Commit behavioral change
- [ ] Next increment
```

Always write one test at a time. Always run all tests (except long-running) after each change.
