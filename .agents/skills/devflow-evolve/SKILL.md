---
name: devflow-evolve
description: Aggregates devflow retrospective improvement proposals scattered across repos, then applies user-approved ones back into the devflow skill files. Closes the retrospective improvement loop. Reads each repo's .devflow/*/retrospective.md, dedupes against a central ledger, and edits only .agents/skills/devflow/** after explicit approval. Use when you want to review and apply accumulated devflow self-improvement proposals.
allowed-tools: Read, Write, Edit, Glob, Bash, AskUserQuestion
user-invocable: true
---

# devflow-evolve

devflow's Step 9 retro-agent writes `## Improvements` proposals into each run's
`retrospective.md` but never applies them. This skill collects those proposals
across repos and applies the approved ones to the live devflow skill files.

Run interactively in the main context (approval needs AskUserQuestion). Never
auto-apply. Edit only files under the live devflow skill directory.

## Inputs and locations

- Scan root: `$SCAN_ROOT` (default `~/ghq`; override via the skill argument)
- Proposals: `{scan_root}/**/.devflow/*/retrospective.md`
- Ledger (single memory source): `$HOME/.devflow/evolve-ledger.jsonl` (append-only JSONL; create dir if missing)
- Apply target: `{devflow_skill_dir}` = the directory containing devflow's `SKILL.md`. Resolve once: it is the sibling `devflow/` of this skill — `{this_skill_dir}/../devflow`. Verify `{devflow_skill_dir}/SKILL.md` exists before applying anything.

## Pipeline

### Step 1: Collect

1. Resolve `{scan_root}` (argument or `~/ghq`).
2. List candidate files:
   ```bash
   find {scan_root} -path '*/.devflow/*/retrospective.md' 2>/dev/null
   ```
3. For each file, compute a content hash:
   ```bash
   shasum -a 256 {file} | cut -c1-12
   ```
4. Read each file and parse the section whose heading starts with `## Improvements` (the retrospective template heading is `## Improvements (proposals only — no auto-apply)`, so match the prefix, not the exact line). For each `### {proposal_title}` block, extract the bold-bulleted fields `- **Target**:`, `- **Problem**:`, `- **Proposed change**:` (capture the text after each label). Skip a file whose Improvements body is `特になし` or empty (but still record a `seen` row in Step 5).
5. Record per proposal: `{run_dir, source_repo, date, proposal_title, target, problem, proposed_change, file_hash}`. `run_dir` = absolute path of the directory containing the retrospective.md, with **no trailing slash** (the plain `dirname` form). This exact string is the dedup key used in Step 2 and written in Step 5 — never add or strip a trailing slash when matching or recording, or already-decided proposals will silently resurface. `source_repo` = the repo root that sits above the `.devflow/` segment in that path. `date` = the `Date:` line inside the retrospective, falling back to the timestamp embedded in the run_dir name.

### Step 2: Exclude against ledger

1. Read `$HOME/.devflow/evolve-ledger.jsonl` (treat missing as empty).
2. For each scanned `run_dir`:
   - If a `seen` row exists with matching `hash`:
     - Keep only proposals whose `(run_dir, proposal_title)` has a `decision` row of `deferred`. Drop `applied` / `rejected`.
     - If no deferred proposals remain, skip the whole run.
   - If no `seen` row, or the `seen` hash differs (file regenerated/edited): treat as first-seen — keep all its proposals.

### Step 3: Aggregate

1. Drop any proposal whose `target` does not resolve under `{devflow_skill_dir}` (see "Target resolution"). Also drop any proposal whose resolved target is a **protected file** — the retro-agent prompt (`prompts/retrospective.md`) or the retrospective template (`templates/retrospective.md`) — even though it resolves in-scope, since the Rules forbid editing them; this skill must not rewrite its own input. Note both kinds of dropped proposals (out-of-scope / protected) to report at the end.
2. Group surviving proposals by resolved target file.
3. Within each group, count how many distinct runs proposed it — this is the priority hint.
4. Order groups by descending run-count (most-proposed targets first). Do NOT auto-merge similar proposals; the human dedupes.

### Step 4: Review and apply

For each group (highest run-count first), present its proposals one at a time:

```
Target: {resolved path}#{section}   ({n} run(s) touched this target)
Problem: {problem}
Proposed change:
{before/after sketch}
```

Then AskUserQuestion with options: Apply / Edit-then-apply / Defer / Reject.
- Apply: open the target file, make the change via Edit, then summarize the actual edit.
- Edit-then-apply: ask the user for the adjusted wording, then Edit.
- Defer: leave for next run (records `deferred`).
- Reject: never resurface (records `rejected`).

Apply edits one proposal at a time. If two proposals touch the same lines, apply the first and re-show the second against the updated file.

### Step 5: Record and report

1. Append to the ledger. Create the directory first if missing (`mkdir -p "$HOME/.devflow"`), then append each row with Bash redirection (`printf '%s\n' '{...}' >> "$HOME/.devflow/evolve-ledger.jsonl"`). Never use the Write tool for the ledger — Write replaces the whole file and would erase all prior "already reviewed" memory.
   - one `seen` row per run_dir whose ledger has **no matching-hash `seen` row yet** — i.e. first-seen runs plus runs whose retrospective hash differs from its last `seen` row. Do NOT re-append a `seen` row for a run that already has a `seen` row with a matching hash, even when it was reviewed this pass because a deferred proposal resurfaced (a matching hash means the row is already current). `proposal_count` = the number of proposals parsed from this retrospective's `## Improvements` section, counted before scope filtering (`特になし`/empty = 0). Row: `{"type":"seen","run_dir":...,"hash":...,"proposal_count":N,"seen_at":ISO8601}`
   - one `decision` row per reviewed proposal. `summary` = for `applied`, a one-line description of the actual edit made; for `deferred`/`rejected`, a one-line reason (may be terse). Row: `{"type":"decision","run_dir":...,"proposal_title":...,"decision":"applied|rejected|deferred","decided_at":ISO8601,"summary":...}`
2. Report a summary: applied / deferred / rejected counts, list of edited files, and any proposals dropped for out-of-scope targets.
3. Do NOT commit. Tell the user to review `git diff` under `{devflow_skill_dir}` and, if they want behavioral validation of a changed prompt, suggest the empirical-prompt-using skill (do not run it).

## Target resolution

Proposals use skill-relative targets such as `.agents/skills/devflow/SKILL.md`,
`devflow/prompts/retrospective.md`, or a bare `SKILL.md` / `prompts/...`.
Map them onto `{devflow_skill_dir}`:
- strip a leading `.agents/skills/devflow/` or `devflow/` prefix, then join the remainder onto `{devflow_skill_dir}`
- a bare `SKILL.md` / `prompts/...` / `templates/...` / `reference/...` joins directly
A target that matches none of the prefixes above is dropped as out-of-scope. After joining, the resolved path MUST be inside `{devflow_skill_dir}` (no `..` escape) and MUST exist (unless the proposal explicitly creates a new file under devflow/). Otherwise drop the proposal as out-of-scope.

## Ledger schema

```jsonl
{"type":"seen","run_dir":"/abs/.../.devflow/2026-06-27T..._foo","hash":"ab12cd34ef56","proposal_count":3,"seen_at":"2026-06-28T10:00:00Z"}
{"type":"decision","run_dir":"/abs/.../.devflow/2026-06-27T..._foo","proposal_title":"inline DoD slice","decision":"applied","decided_at":"2026-06-28T10:01:00Z","summary":"..."}
```

Identity: a retrospective is keyed by its `run_dir` absolute path (timestamped,
unique, written once). A proposal is keyed by `(run_dir, proposal_title)`.
`hash` guards against the rare case the file was hand-edited/regenerated.

## Rules

- Never auto-apply; every edit needs explicit approval
- Edit only files resolving under `{devflow_skill_dir}`; drop out-of-scope targets
- Never commit; the user reviews the dotfiles git diff
- Ledger is append-only and the single source of "already reviewed" memory — append with Bash `>>`, never the Write tool
- Do not modify retro-agent, the retrospective template, or any other skill
