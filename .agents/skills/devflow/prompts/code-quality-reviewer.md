<!-- Orchestrator-only: dispatch metadata (not part of the agent's instructions)
- subagent_type: review-agent
- model: opus
- placeholders: {cwd}, {target_dir}, {what_was_implemented}
- before dispatch, prepare {target_dir}: `cd {cwd} && ~/.claude/skills/reviewing-code/scripts/prepare_target.sh {target_dir} local --base {base_sha} {target_files}`, where {base_sha} is the commit the reviewed range starts from and {target_files} is the shell-quoted list of files the range changed (a listed file with no change would be reviewed whole)
-->

---

Review the prepared change with the reviewing-code skill.

Working directory: {cwd}
Prepared target directory (target kind `local`): {target_dir}
What was implemented: {what_was_implemented}

Also check whether each file keeps a single responsibility, can be tested on its own, and has not grown past what its responsibility needs.

IMPORTANT: End your review with a machine-readable summary block in exactly this format, counting the final findings by disposition:

---SUMMARY---
BLOCKER: {count}
NEEDS_DECISION: {count}
QUESTION: {count}
FOLLOW_UP: {count}
NITS: {count}
---END---
