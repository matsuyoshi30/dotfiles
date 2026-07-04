---
description: Review staged git changes
---
Review the staged changes (`git diff --cached`; if nothing is staged, review `git diff` instead). Focus on:

- Bugs and logic errors
- Security issues (injection, secrets, unsafe input handling)
- Error handling gaps
- Missing test coverage for the changed behavior

Report findings ordered by severity, referencing file and line. If nothing significant is found, say so briefly instead of inventing issues.
