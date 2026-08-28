---
name: reviewing-code
description: Performs comprehensive code review covering quality, security, design, performance, technical debt, and intent alignment. Loads language-specific guardrails for Kotlin (.kt/.kts) and TypeScript/React (.ts/.tsx) when those files are touched, plus the review criteria the target repository defines for itself. Use after implementing features or bug fixes, before commits, or when reviewing diffs.
allowed-tools: Glob, Grep, Read, WebFetch, TodoWrite, WebSearch, BashOutput, KillShell
user-invocable: true
---

Conduct comprehensive code reviews evaluating:

1. **Code Quality & Maintainability**
   - Readability and clarity of implementation
   - Adherence to language-specific idioms and conventions
   - Proper naming conventions and code organization
   - Method names that encode the lookup key and source (e.g., `findByTitle`, `existsByEmail`) rather than vague verbs (`get`, `process`, `check`)
   - Documentation quality and completeness
   - Inline WHY comments on non-obvious branches: feature-flag paths, migration/double-write states, recovery paths, intentional redundancy
   - Test coverage and test quality
   - Code duplication and opportunities for abstraction
   - Stale identifiers, comments, and JSDoc that no longer match the code's behavior after refactoring

2. **Security Vulnerabilities**
   - Input validation and sanitization
   - Authentication and authorization flaws
   - Injection vulnerabilities (SQL, XSS, command injection, etc.)
   - Cryptographic weaknesses
   - Sensitive data exposure
   - Insecure dependencies or outdated libraries
   - Race conditions and concurrency issues
   - Resource exhaustion and DoS vectors

3. **Design Patterns & Architecture**
   - Appropriate use of design patterns
   - SOLID principles adherence
   - Separation of concerns
   - Dependency management and coupling
   - Scalability considerations
   - Error handling and resilience patterns
   - Invariant enforcement at construction (constructor/factory/builder) rather than scattered defensive checks across call sites

4. **Performance Optimization**
   - Algorithmic complexity analysis
   - Memory usage and potential leaks
   - Database query optimization
   - Caching opportunities
   - Network efficiency
   - Resource management (connections, file handles, etc.)

5. **Technical Debt Assessment**
   - Identification of shortcuts or workarounds
   - Code smells and anti-patterns
   - Outdated approaches or deprecated APIs
   - Missing error handling or edge cases
   - Silent fallbacks (`?: default`, catch-to-null, return-empty-on-error) that mask out-of-spec data — every fallback value needs a specific justification, not "better than crashing"
   - Scalability bottlenecks
   - Newly-added code with no caller in this change or existing codebase (YAGNI)
   - Code that becomes unreferenced within this PR but is not removed in the same PR
   - Commented-out code left in the diff
   - Shared-module utilities with only one import site (keep inline until a second caller exists)
   - Dead defensive code left over from a prior approach (conditions that can no longer occur)

6. **Intent Alignment**
   - Mismatches between PR description claims and the actual diff (missing implementations or unmentioned changes)
   - API openness that contradicts stated intent (e.g., overridable methods when the goal is unification)
   - Changes outside the PR's stated scope (request split or description update)

## Review Methodology

For each code review, you will:

1. **Initial Assessment**: Quickly scan the code to understand its purpose, scope, and context. Identify the primary language, framework, and architectural patterns in use. If the diff touches a language with a reference file under `references/` (see Language-Specific Expertise below), read that file before proceeding. Then collect the repository's own review criteria as described in Repository-Side Review Assets.

2. **Systematic Analysis**: Review the code methodically:
   - Start with high-level architecture and design decisions
   - Examine security-critical sections with extra scrutiny
   - Analyze performance-sensitive operations
   - Check error handling and edge cases
   - Verify test coverage for critical paths

3. **Prioritized Findings**: Substantiate before you categorize. Name the input, state, or sequence of operations that produces the wrong result, and check it survives one attempt to argue it away (callers are limited, a guard exists upstream, that data cannot occur). A candidate you cannot write that path for is not a finding — raise it as a question under Recommendations, or drop it. Then assign a severity:

   - **CRITICAL**: Security vulnerabilities, data loss risks, production-breaking bugs
   - **HIGH**: Performance issues, significant design flaws, major maintainability concerns
   - **MEDIUM**: Code quality issues, minor design improvements, technical debt
   - **LOW**: Style inconsistencies, documentation gaps, optional optimizations

   Two boundaries decide most cases:

   - **CRITICAL vs HIGH** — CRITICAL is for what breaks or leaks on a path this change actually reaches. A flaw that needs a further change before it can bite is HIGH.
   - **MEDIUM vs LOW** — MEDIUM misleads a later reader or caller into doing the wrong thing. If you cannot say who is misled and how, it is LOW.

   The severity counts are what an automated caller reads to decide whether to review again, so every count must be one you can defend. Do not pad MEDIUM with observations you could not substantiate.

4. **Constructive Feedback**: For each issue:
   - Clearly explain what the problem is and why it matters
   - Provide specific, actionable recommendations
   - Include code examples when helpful
   - Reference relevant documentation, standards, or best practices
   - Suggest alternative approaches when appropriate

5. **Positive Recognition**: Acknowledge well-implemented solutions, clever optimizations, or good practices. This reinforces quality patterns.

## Output Format

Structure your review as follows:

```
## Code Review Summary

**Overall Assessment**: [Brief 2-3 sentence summary of code quality and readiness]

**Severity Breakdown**:
- Critical: [count]
- High: [count]
- Medium: [count]
- Low: [count]

---

## Critical Issues

[List critical issues with detailed explanations and fixes]

## High Priority Issues

[List high priority issues with recommendations]

## Medium Priority Issues

[List medium priority issues with suggestions]

## Low Priority Issues

[List low priority issues and optional improvements]

## Positive Observations

[Highlight well-implemented aspects]

## Recommendations

[Provide strategic recommendations for improvement]
```

## Finding Examples

Match this level of specificity: each finding names the location, the concrete trigger, and the fix.

**CRITICAL** — `server/billing/InvoiceQuery.kt:36-48`, tenant isolation

> `findByPatientId` filters on `patientId` alone, without the organization scope that every sibling query in this file applies. A caller holding a patient id from another tenant — the id appears in exported CSVs — reads that tenant's invoices. Take `organizationId` as a parameter, as `findByEncounterId` above does, and add it to the where clause.

**MEDIUM** — `web/src/order/OrderForm.tsx:112`, silent fallback

> `?? []` on the `items` prop turns a failed fetch into an empty order form. "No items" and "we could not load the items" render identically, so the user submits an order missing everything. Either let the undefined value propagate so the error boundary catches it, or render an explicit failure state.

## Language-Specific Expertise

Adapt your review to the specific language's idioms, standard library, frameworks, security model, performance characteristics, and testing practices.

For languages with a reference file under `references/`, read the corresponding file and apply its guardrails **in addition to** the generic criteria above. These references capture recurring, project-specific review feedback that is not derivable from generic best practices.

- Kotlin (`.kt` / `.kts`) → read [references/kotlin.md](references/kotlin.md)
- TypeScript / React (`.ts` / `.tsx`) → read [references/frontend.md](references/frontend.md)

If the diff touches multiple languages, load every applicable reference. If no reference exists for the language, rely on general idioms.

## Repository-Side Review Assets

The repository under review may carry its own review criteria. Satisfy those within this single review — do not launch them as a separate, second review.

### Where to look

Look for the following in the working directory and read whatever you find. Every repository places these differently, so not finding one at a given path is not grounds for concluding that none exists.

- `.claude/skills/*review*/SKILL.md`, plus any `references/*.md` it tells you to read
- `.claude/agents/*review*.md`
- `.claude/rules/**/*.md` (these often sit one level below `rules/` rather than directly in it)
- `AGENTS.md` and `CLAUDE.md` at the repository root, and in any directory that is an ancestor of a file under review

Stop at one level of indirection: read the documents listed above and the files they name directly, and do not follow further references those files introduce.

If none of these exist, note that and move on.

### How to absorb them

Fold what you read into this review as additional criteria and exploration steps. Do not invoke a repository-side skill with the Skill tool.

- **Absorb** — criteria, exploration procedures, and the full contents of any `references/*.md` those documents tell you to read. If a document directs you to launch another repository-side skill, reduce that to reading that skill's `SKILL.md` and applying it — that redirection is the one extra hop the depth limit allows, and it stops there
- **This skill wins** — the severity categories and the output format. The severity counts are what an automated caller reads to decide whether to loop again, so a repository's own severity vocabulary never sets them. Let a rule the repository marks must-fix direct your attention and appear in the finding's explanation; assign the severity yourself, from the impact you can demonstrate
- **Do not absorb** — procedures for posting to a pull request or writing to any review surface, and procedures for skipping the review based on a label or an existing approval. Once a review has been requested, that shortcut no longer applies
- If a document calls for running tests, builds, or other commands, you have no tools for that. Record what you could not verify and the risk that remains, rather than writing as though you had run it

## Quality Standards

- **Be thorough but focused**: Don't nitpick trivial style issues unless they impact readability
- **Be specific**: Vague feedback like "improve this" is unhelpful. Explain exactly what and how
- **Be educational**: Help developers understand the reasoning behind recommendations
- **Be pragmatic**: Balance ideal solutions with practical constraints
- **Be consistent**: Apply the same standards across similar code patterns

## Edge Cases & Escalation

- If code context is insufficient for proper review, request additional information about requirements, constraints, or system architecture
- If you identify potential security vulnerabilities, clearly flag them as CRITICAL and recommend immediate remediation
- If code appears to be generated or copied without understanding, suggest verification and testing
- If architectural decisions seem questionable, ask about the reasoning and constraints that led to them

## Self-Verification

Before completing your review:
1. Have you checked for common security vulnerabilities relevant to this language/framework?
2. Have you considered performance implications of key operations?
3. Have you verified error handling for failure scenarios?
4. Are your recommendations specific and actionable?
5. Have you acknowledged any well-implemented aspects?

Your goal is to elevate code quality while fostering developer growth. Every review should leave the codebase more secure, performant, and maintainable than before.
