<!-- Orchestrator-only: dispatch metadata (not part of the agent's instructions)
- subagent_type: review-agent
- model: opus
- placeholders: {cwd}, {target_files}, {what_was_implemented}, {base_sha}, {head_sha}
-->

---

Review the following files in the working directory: {cwd}

**What was implemented:** {what_was_implemented}
**Files to review:** {target_files}
**Git range:** {base_sha}..{head_sha}

Review the diff (`git diff {base_sha}..{head_sha}`) and all changed files using the dimensions below.

## Review Dimensions

1. **Code Quality** — Readability, idioms, naming (encode lookup key: `findByTitle` not `get`), WHY comments on non-obvious branches, test quality, stale identifiers after refactoring
2. **Security** — Input validation at boundaries, injection vulnerabilities, sensitive data exposure, race conditions
3. **Design** — SOLID, separation of concerns, invariant enforcement at construction, error handling
4. **Performance** — Algorithmic complexity, memory leaks, N+1 queries, resource management
5. **Technical Debt** — Silent fallbacks masking bad data (every fallback needs justification), YAGNI violations (no caller), unreferenced code not removed, commented-out code, single-use shared utilities, dead defensive code
6. **Intent Alignment** — Mismatches between stated implementation and diff, API openness contradicting intent, out-of-scope changes

Also check: single responsibility per file, independent testability, file size growth.

For each Critical/High/Medium issue: file path, line number, what's wrong, why it matters, specific fix.

Be strict. Don't say "looks good" without thorough examination. Do acknowledge genuine strengths.

---SUMMARY---
CRITICAL: {count}
HIGH: {count}
MEDIUM: {count}
LOW: {count}
---END---
