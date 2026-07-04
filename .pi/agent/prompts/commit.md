---
description: Draft a commit message for staged changes
---
Look at the staged changes (`git diff --cached`, `git status`) and draft a commit message.

- Match the style and language of recent commits (`git log --oneline -10`)
- Subject line only, unless the change genuinely needs a body
- If the staged changes mix unrelated concerns, propose how to split them instead

Do not commit. Show the message and wait for approval.
