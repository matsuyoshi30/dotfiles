---
name: shaping-screens
description: Shape a vague UI idea into a runnable Astro + HTML/CSS prototype through dialogue. Use when screens, transitions, and states should be settled before implementation, for either a new web app or an extension to an existing repo. Combines external design-system references (via WebFetch) with existing-repo token extraction (via Grep/Glob), and optionally captures existing screens via playwright-skill or browser-use.
allowed-tools: Read, Write, Edit, Glob, Grep, Bash, WebFetch, TodoWrite, Agent(Explore), Skill(playwright-skill, browser-use, superpowers:writing-plans, devflow, advisor-critique-loop)
user-invocable: true
---

# Shaping Screens

Shape a vague UI request into a runnable Astro prototype through dialogue. UI-focused counterpart to `shaping-spec`.

**Announce at start:** "Using shaping-screens skill to shape your UI idea into an interactive Astro prototype."

## Positioning vs Adjacent Skills

- **superpowers:brainstorming** - codebase-agnostic design dialogue. This skill is codebase-aware.
- **shaping-spec** - text-centric functional spec. Use when screen-level detail is not needed.
- **shaping-screens (this skill)** - UI-centric. Use when screens, transitions, and states should be settled first.

If both text-spec and screen-spec are needed, the user runs them serially (recommended order: shaping-spec then shaping-screens).

## Output Location

Artifacts are written under `{prototype_dir}` — the absolute path resolved in Step 4.2. By default this expands to `{base_dir}/screens/{YYYY-MM-DDTHH-MM-SS}_{slug}/`, but Step 4.2 normalises away duplicate `screens` segments when the user supplies a `{base_dir}` that already ends in `/screens`.

`{base_dir}` resolution algorithm:

```
1. Build the file list F:
   - {project_root}/CLAUDE.md  ({project_root} = git top-level of target_repo,
     or cwd's git top-level if no target_repo)
   - $HOME/.claude/CLAUDE.md
   For each file in F, also include any file referenced via @filename.md
   imports (one level deep).

2. Build the candidate path list P (preserve textual order across files):
   For each line L in each file in F:
     if L contains ANY of [notes, specs, work, save, 保存, ディレクトリ,
                            location, output]
     AND L contains a path token matching /^[/~.]/ OR `…` (backticks):
        extract the path token, expand $HOME and ~, append to P.

3. Walk P in order; return the first path that exists and is a directory.

4. If P is empty or none exist, fall back to $HOME/.shaping-screens/
   (create it if missing).
```

`{slug}` is a kebab-case identifier (max 50 chars). Propose it to the user in Step 4 and confirm.

`{YYYY-MM-DDTHH-MM-SS}` uses local time (e.g. `2026-04-21T15-30-42`). Uniqueness at 1-second granularity is assumed; on the rare collision, retry with a fresh timestamp.

## Skill Self-Reference

`{skill_dir}` is the absolute path of the directory holding this `SKILL.md`. Resolve it **once** at the start of the run, in this order:

1. If the harness exposes a skill-load path (e.g. via env var or tool argument), use it.
2. Otherwise, check `$HOME/.claude/skills/shaping-screens/` and the project-local fallback `<repo>/.agents/skills/shaping-screens/` (resolve `<repo>` against cwd's git top-level). Use the first that exists and contains this `SKILL.md`.
3. If neither resolves, ask the user for the absolute path to the skill directory and stop until answered.

All later references to `{skill_dir}/templates/...` resolve against that absolute path, regardless of cwd or target-repo path.

## Flow

At the start, register the following checklist via TodoWrite and tick items off as you go:

```
shaping-screens progress:
- [ ] Step 0: Receive input + detect mode (external_ds, existing_repo, scope_dir)
- [ ] Step 1: Gather inputs (External-DS and/or Existing-repo sub-flows)
- [ ] Step 2: Ground-truth confirmation
- [ ] Step 3: Screen-definition dialogue (sitemap / transitions / states / opt-in)
- [ ] Step 4: Propose slug -> expand Astro starter -> user approval
- [ ] Step 5: Recommend downstream -> user choice -> invoke or stop
```

### Step 0 - Receive Rough Idea + Mode Detection

Accept the input (inline text, file path, or GitHub issue URL):
- Issue URL: `gh issue view <url> --json title,body,labels`
- File path: `Read`
- Inline text: use as-is

Summarise what you understood in one sentence and confirm with the user.

**Mode is detected on two independent axes plus an optional scope.** Ask each question in its own turn (one question at a time):

1. **External-DS axis** - "Will this prototype reference any external design systems (Material 3, Atlassian, shadcn, etc.)? (Y/N — multiple URLs allowed if Y.)" Hold the answer as `external_ds: bool`.
2. **Existing-repo axis** - "Should this prototype align with an existing repository's tokens / components? (Y/N.)" Hold as `existing_repo: bool`. If Y, ask once more for the **absolute path of the target repository** (default cwd if it is a git working tree).
3. **Scope axis** (only if `existing_repo` is Y) - "Is there a sub-directory you want me to scope the existing-repo search to? (e.g. `packages/ui`. Leave blank for the whole repo.)" Hold as `scope_dir: string | null`.

A pre-existing `git rev-parse --show-toplevel` in cwd is a **hint**, not a decision: if it succeeds the user usually means `existing_repo = Y` with that repo, but always confirm with the questions above.

Record the resulting tuple `{external_ds, existing_repo, target_repo, scope_dir}` in working memory; Step 1 dispatches to one or both sub-flows based on the booleans. The legacy phrasing "new mode" maps to `(external_ds=Y or N, existing_repo=N)` and "existing mode" maps to `(existing_repo=Y)`. The `## Mode` section of `spec.md` records the tuple verbatim.

Never proceed without confirmation. Do not move to Step 1 until both axes are answered.

### Step 1 - Input Gathering

Run the sub-flows that match the Step 0 tuple. If both `external_ds` and `existing_repo` are Y, run the External-DS sub-flow first, then the Existing-repo sub-flow, and merge findings in working memory before Step 2. If both are N, skip Step 1 entirely and rely on the user's prose alone (record "no references" in the Context section of `spec.md`).

#### External-DS sub-flow (when `external_ds = Y`)

1. Ask the user for URLs of external design systems to reference. Multiple are allowed (e.g. Material 3, Atlassian, shadcn). **Skip this question if the user already provided URLs while answering the Step 0 external_ds axis** — reuse those URLs directly.
2. For each URL, run `WebFetch` and summarise component naming, tone, and base tokens (color / spacing / typography).
3. On failure:
   - 404 / timeout: ask the user to fix the URL or skip that DS.
   - WebFetch returns an empty page or near-empty content (typical for SPA-rendered docs sites): ask the user whether to escalate to `Skill(browser-use)` for a rendered fetch, or to skip the DS.
   - All URLs fail, or the user opts to reference nothing: continue from the user's prose alone. Record "no DS referenced" in the Context section of `spec.md`.

#### Existing-repo sub-flow (when `existing_repo = Y`)

1. **Always** Grep/Glob the target repo (read-only). If `scope_dir` is set, prepend it to every Glob base so the search stays inside that sub-directory:
   - CSS / tokens: `{scope_dir or ''}/**/{*.css,*.scss,tailwind.config.*,tokens.*,theme.*,design-tokens.*}`
   - Components: `{scope_dir or ''}/**/{components,ui,design-system}/**/*.{tsx,jsx,vue,svelte,astro,html}`
   - Storybook etc: `{scope_dir or ''}/**/*.stories.*`
2. Propose escalation to an Explore subagent if any of the following hold (evaluate **within `scope_dir`** when set):
   - Candidate files span 10+ paths
   - Hits scatter across 3+ packages / top-level directories
   - Unfamiliar frameworks / libraries dominate the area
   - The user explicitly asks for a deeper survey
3. On approval, dispatch `Agent(Explore)` with `thoroughness: "medium"`. Fold the returned summary into working memory.
4. **0-hit handling.** If steps 1-3 yield zero relevant matches after filtering out:
   - third-party caches and build outputs: `node_modules/`, `vendor/`, `.git/`, `.idea/`, `.vscode/`, `.emacs.d/`, `dist/`, `build/`
   - any path inside `{skill_dir}` (the skill's loaded location)
   - **any path matching `**/.agents/skills/shaping-screens/**` or `**/.claude/skills/shaping-screens/**`** within `target_repo` — covers cases where this skill's source / mirror sits inside the target repo (e.g. dotfiles repos that store skills under `.agents/skills/`)
   
   then present **three** options to the user — do not silently fall back, and do not assume "switch to new mode":
   - (a) **Path / scope wrong** - confirm `target_repo` and `scope_dir`, re-run from step 1.
   - (b) **Switch to no-existing-repo mode** - flip `existing_repo` to N for this session and continue with whatever External-DS findings exist (or prose only).
   - (c) **Continue with template defaults** - keep `existing_repo = Y` but accept that no tokens were extracted. Record `Context: "no design tokens or components extracted from {target_repo}"` in `spec.md`. The Astro starter's default `global.css` values are used unchanged.
5. **Visual capture (opt-in):**
   - Always ask explicit consent: "Should I capture screenshots of the existing screens? (Requires a dev server already running — I will not start it for you. Tell me the URL once it is up.)"
   - If yes: confirm the **already-running** target URL with the user, then invoke `Skill(playwright-skill)` or `Skill(browser-use)`. Screenshots land in `{prototype_dir}references/screenshots/`. Because the directory is created in Step 4, in Step 1 only **plan** the capture; perform the actual capture after Step 4 has created the directory.
   - If no: skip. Record "no visual capture" in the Context section of `spec.md`.
6. Hold a summary of findings (candidate design tokens, main components, list of existing page URLs) in working memory. Present it to the user in Step 2.

**Read-only invariant:** Step 1 never writes to the target repo. Use only Grep / Glob / Read / WebFetch.

### Step 2 - Ground-Truth Confirmation

Present the findings from Step 1 (candidate design tokens, list of existing components, or the tone of the external DS) back to the user in 1-2 questions, asking whether your understanding is correct.

Example:
> "I extracted these design tokens from the existing repo: `--color-accent: #2563eb`, `--space-3: 16px`, ... Is this right? Let me know if anything should be added or corrected."

If anything is off, loop back to Step 1.

### Step 3 - Screen Definition Dialogue

Ask **one question at a time**. Prefer **2-4 multiple-choice options + your recommendation + a one-line reason**.

#### 3.1 Sitemap

> "Is the screen list correct? A. Login / B. Index / C. Detail / D. Other (free text)"

Drill in until the user settles the list.

#### 3.2 Transitions

For each transition:
> "What triggers `/list` -> `/detail/:id`? A. Row click (recommended: most common) / B. Button inside the row / C. Other"

Loop until every transition is captured. They will be written to `transitions.json` later.

#### 3.3 State coverage

For each screen:
> "The default states for `/list` are loading / empty / error / success. Any you want to opt out of? (e.g. empty is not needed for this feature.)"

The default is to include all four.

#### 3.4 Opt-in sections

> "Which of the following should the spec also cover? (multi-select; skip if none):
> 1. Interaction detail (hover / click / keyboard)
> 2. Responsive (mobile / tablet / desktop)
> 3. Accessibility (ARIA / contrast)
> 4. Roles / permissions
> 5. i18n"

Ask follow-up questions only for the items the user selected, and write the results into the corresponding sections of `spec.md`. Sections that were not selected MUST be deleted from `spec.md` (do not leave empty section headings).

**Outputs of Step 3:**
- Sitemap (screen list)
- Transition list (from / to / trigger / expected state)
- State set per screen
- Content for each opt-in section

### Step 4 - Slug -> Template Expansion -> Approval

#### 4.1 Propose slug

Derive a kebab-case `{slug}` (max 50 chars) from the user's idea and confirm:
> "Slug proposal: `login-error-ux`. OK, or do you have an alternative?"

#### 4.2 Confirm output location

Resolve `{base_dir}` per the "Output Location" section and present it:
> "Target path: `~/.matsuyoshi30/screens/2026-04-21T15-30-42_login-error-ux/`. OK? Let me know if you'd like a different path."

If the user supplies a path, override `{base_dir}` with it.

**Path normalisation — derive `{prototype_dir}`.** Once `{base_dir}` is settled, compute the single variable `{prototype_dir}` that every later step (4.3 onward) writes into. Do **not** re-append `screens` after this point.

1. Strip a single trailing `/` from `{base_dir}` if present (so `~/x/screens/` and `~/x/screens` are treated identically).
2. If the **last path segment** of `{base_dir}` is exactly `screens` (i.e. `path.basename(base_dir) == "screens"`, not just `endswith("screens")` — `~/my-screens` does NOT match), set `{prototype_dir}` = `{base_dir}/{TS}_{slug}/`.
3. Otherwise, set `{prototype_dir}` = `{base_dir}/screens/{TS}_{slug}/`.

If `{prototype_dir}` is inside the target repo (existing-repo mode), warn once: *"This will write inside the target repo at `<path>`. Is that intentional? (The read-only invariant still holds — only this sub-directory is written.)"* and wait for confirmation before mkdir.

#### 4.3 Create the directory

```bash
mkdir -p {prototype_dir}
```

If it already exists, retry with a fresh timestamp (1-second collisions are not expected in practice).

#### 4.4 Copy the Astro starter

```bash
cp -r {skill_dir}/templates/astro-starter/. {prototype_dir}
```

The Astro starter no longer contains `_example-screen.astro` (it lives at `{skill_dir}/templates/_example-screen.astro`), so the copy is naturally clean.

#### 4.5 Generate screen pages

Read the example template via `Read({skill_dir}/templates/_example-screen.astro)` and, for each screen from the Step 3 sitemap, write a copy to `{prototype_dir}src/pages/{screen-name}.astro`:
- Fill in the screen name, the state set, and the opt-in content; rewrite the `StateSwitcher` slots.
- Drop slots for states the user opted out of (do not leave empty `<slot name="...">` placeholders).
- For nested routes (e.g. `/skills/[name]` → `src/pages/skills/[name].astro`), `mkdir -p` the parent directory before writing.
- Inject the design tokens (collected in Step 1) into `:root` inside `{prototype_dir}src/styles/global.css`.

#### 4.6 Update the sitemap

Overwrite the `screens` array inside `{prototype_dir}src/pages/index.astro` with the generated screen list.

#### 4.7 Generate transitions.json

```json
{
  "transitions": [
    { "from": "/login", "to": "/dashboard", "trigger": "Sign in button click", "state": "success" }
  ]
}
```

Write it to `{prototype_dir}src/transitions.json`.

#### 4.8 Generate spec.md / README.md

Fill `{skill_dir}/templates/spec.md.tmpl` and `{skill_dir}/templates/README.md.tmpl` with the Step 1-3 results and write them to `{prototype_dir}spec.md` and `{prototype_dir}README.md`.
- The `## Goal` body is the one-sentence summary you confirmed at the start of Step 0 (verbatim or lightly edited).
- The `## Mode` body is the tuple from Step 0 verbatim, e.g. `external_ds=Y, existing_repo=Y, target_repo=/path/to/repo, scope_dir=packages/ui` (or `null` for any axis that wasn't set).
- The `## Prototype Location` body is `{prototype_dir}` resolved to its absolute path.
- Delete entire opt-in sections that were not selected.
- `## Downstream` MUST always be kept (Step 5 will fill it in).

#### 4.9 Run the visual capture (only if opt-in was granted)

For each URL noted in Step 1, invoke `Skill(playwright-skill)` or `Skill(browser-use)` and save screenshots under `{prototype_dir}references/screenshots/`.

#### 4.10 User approval

> "Expansion complete: `{prototype_dir}`.
> To run: `cd {prototype_dir} && pnpm install && pnpm dev` then open http://localhost:4321/.
> Tell me what to fix, or say OK to proceed."

Apply edits with the Edit / Write tools. After every change, ask the user again.

**Do not move to Step 5 until 4.10 is approved.**

### Step 5 - Downstream Routing

Recommend a downstream skill and let the user choose:

| Suggest | When |
|---|---|
| **superpowers:writing-plans** | Screens / transitions are settled, but the implementation approach is open. |
| **devflow** | Adding to existing code is a straight extension; the approach has a clear precedent. |
| **advisor-critique-loop** | Wide blast radius; multi-model review adds value. |
| **Stop here** | The user prefers to hand the spec off manually. |

> "Recommended downstream: **{skill}** - {one-line reason}. Alternatives: {others}. Or stop here. Which?"

Always record the user's choice in the `## Downstream` section of `spec.md` (record "Stop here" verbatim if chosen).

Invoke the chosen skill via the `Skill` tool, passing **the absolute path to `{prototype_dir}spec.md`** as `args` (pass the summary spec, not the entire prototype directory).

If "Stop here" is chosen: tell the user the spec path and the recommended skill, then end the session.

## Dialogue Principles

- **One question at a time** - never stack multiple open points in a single message.
- **Multiple choice preferred** - 2-4 options with your recommendation and a one-line reason.
- **Ground-truth before shaping** - do not skip Step 2.
- **YAGNI on optional sections** - opt-in items are added only when the user explicitly selects them.
- **Do not choose the approach here** - shaping collects options; the downstream skill picks the implementation approach.

## Rules / Invariants

- **No code edits in the target repo** - Steps 0-3 are read-only. The only write target is `{prototype_dir}` (resolved in Step 4.2).
- **No auto install** - `pnpm install` is run by the user. The skill MUST NOT run `npm i -g astro` or similar.
- **Visual capture is opt-in** - even in existing mode, explicit consent is required. Do not invoke `playwright-skill` / `browser-use` without it.
- **`spec.md` always exists at the end** - even on partial failure, leave at least a Goal section.
- **Single hop downstream** - invoke at most one downstream skill per session. If the user wants more, they chain manually.
- **`## Downstream` is mandatory in spec.md** - never delete it; record "Stop here" verbatim with a reason if chosen.

## Error / Exception Paths

| Case | Response |
|---|---|
| Step 0: target repo cannot be determined | Default both axes to N and require explicit user confirmation; offer to set `existing_repo = Y` with a path the user supplies. |
| Step 1: WebFetch fails | Let the user fix the URL, escalate to `Skill(browser-use)` for SPA-rendered pages, or skip that DS. |
| Step 1: Grep/Glob returns 0 hits in existing mode | Present the three options from Step 1 sub-flow item 4: (a) re-confirm path/scope, (b) flip `existing_repo` to N, (c) continue with template defaults. Never silently fall back. |
| playwright-skill / browser-use fails | Skip the visual capture and continue with URL/code exploration only. |
| `{prototype_dir}` already exists | 1-second collisions are not expected; on the rare hit, retry with a fresh timestamp. |
| Write error mid-template-expansion | Do not roll back. Ask the user to clean up manually; note this at the top of `spec.md`. |
| User declines Agent(Explore) escalation | Continue with the lightweight exploration only; record "exploration limited" in `spec.md`'s Open Questions. |
| Opt-in additions arrive after Step 4 | Loop back to Step 3 and update only the affected files. |
| Downstream skill fails to start | Surface the error. The spec path is preserved, so the user can invoke it manually later. |
