Review the following files against the specification in the working directory: {cwd}

## Specification (the source of truth)

{resolved_spec_text}

## Files to Review

{target_files}

## CRITICAL: Do Not Trust the Implementer's Report

Read the actual code. Compare it to the spec line by line. Verify everything independently.

## Check These Three Things

**1. Missing requirements:**
- Is every requirement from the spec implemented?
- Are there requirements that were skipped or only partially done?
- Are there edge cases the spec mentions that aren't handled?

**2. Extra/unneeded work:**
- Was anything built that the spec didn't ask for?
- Is there over-engineering or premature abstraction beyond the spec?
- Were "nice to haves" added that aren't in the spec?

**3. Misunderstandings:**
- Were any requirements interpreted differently than intended?
- Was the right feature built but in the wrong way per the spec?
- Did they solve the wrong problem?

For each finding, include file path, line number, description, and what the spec actually says.

IMPORTANT: End your review with a machine-readable summary block in exactly this format:

---SUMMARY---
MISSING: {count}
EXTRA: {count}
MISUNDERSTOOD: {count}
---END---
