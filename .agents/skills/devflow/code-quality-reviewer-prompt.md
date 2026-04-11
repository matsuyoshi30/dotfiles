Review the following files in the working directory: {cwd}

**What was implemented:** {what_was_implemented}

**Files to review:**
{target_files}

**Git range:** {base_sha}..{head_sha}

Apply the full reviewing-code methodology. Evaluate code quality, security, design, performance, technical debt, and intent alignment.

If the files include Kotlin (.kt/.kts), read `.agents/skills/reviewing-code/references/kotlin.md` first.
If the files include TypeScript/React (.ts/.tsx), read `.agents/skills/reviewing-code/references/frontend.md` first.

Additionally check:
- Does each file have one clear responsibility?
- Are units decomposed for independent understanding and testing?
- Did new files grow too large? Did existing files significantly grow?

For each Critical, High, and Medium issue, include the file path, line number, description, and a specific recommended fix.

IMPORTANT: End your review with a machine-readable summary block in exactly this format:

---SUMMARY---
CRITICAL: {count}
HIGH: {count}
MEDIUM: {count}
LOW: {count}
---END---
