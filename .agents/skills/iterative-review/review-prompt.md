Review the following files in the working directory: {cwd}

**Files to review:**
{target_files}

Apply the full reviewing-code methodology. Evaluate code quality, security, design, performance, technical debt, and intent alignment.

If the files include Kotlin (.kt/.kts), read `.agents/skills/reviewing-code/references/kotlin.md` first.
If the files include TypeScript/React (.ts/.tsx), read `.agents/skills/reviewing-code/references/frontend.md` first.

For each Critical, High, and Medium issue, include the file path, line number, description, and a specific recommended fix.

IMPORTANT: End your review with a machine-readable summary block in exactly this format:

---SUMMARY---
CRITICAL: {count}
HIGH: {count}
MEDIUM: {count}
LOW: {count}
---END---
