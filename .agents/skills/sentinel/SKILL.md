---
name: sentinel
description: Continuously watch for targets matching a natural-language "watch condition" passed as an argument (Linear tickets / GitHub PRs・Issues / Sentry errors / Slack messages / CI・deploy state / any state observable via an MCP tool or shell command). On each detection, spawn a dedicated subagent that first runs a lightweight triage classifying the item as no-action / light-done / needs-implement, and only auto-escalates needs-implement items to devflow --auto, driving them up to (but not past) the irreversible step (Draft PR). Both the watch condition and the action are freely specifiable in natural language; polling self-paces via ScheduleWakeup; each task is delegated to a subagent so the main context stays lean. Fires on "sentinel で〜を監視して", "〜を見張って", "〜が来たら自動で対応して", "/sentinel <watch>", "センチネル回して", etc. Never performs irreversible operations (merge, production rollout); it stops just before them and reports.
user-invocable: true
---

# sentinel

A session-resident sentinel Skill: continuously watch for targets matching the `<watch>` argument, delegate each detection to a dedicated subagent, sort it via a **triage-first action ladder** into no-action / light-done / needs-implement, and auto-escalate only the needs-implement items to `devflow --auto`, driving them **up to but not past the irreversible step**.

Throughout, the three verdicts are written in prose with hyphens (no-action / light-done / needs-implement) and as literal ledger/return tokens with underscores (`no_action` / `light_done` / `needs_implement`) — they refer to the same three classes.

Built on wakwak's autopilot, reshaped for the matsuyoshi environment (devflow / preparing-worktrees / memory / masanyang). Key differences from wakwak's version:

- Instead of jumping straight to devflow on every detection, it enters a lightweight **triage** first (most items settle on a cheap path; heavy devflow only runs for confirmed implementation work).
- The implementation action is **`devflow --auto`**, not `dev-task` (worktree creation is delegated to devflow/preparing-worktrees; the harness's disposable `isolation: "worktree"` is not used).
- State file lives at `~/.matsuyoshi30/sentinel/<slug>.json` (globally gitignored).
- The report channel is a config point (default inline + optional Slack/Discord/memory).
- Adds a first-run seed guard and a concurrency cap as safety devices.

## Watch targets are not limited

Anything observable via polling is fair game:

- Linear tickets (assignee / state / parent's children / label)
- GitHub PRs・Issues (new / review requested / CI failure / comment)
- Sentry new errors・regressions
- Slack specific messages・mentions
- CI / deploy state changes
- Output of any MCP tool・shell command

```
/sentinel <watch>
```

`<watch>` is **freely written in natural language**. It contains the **trigger** (what to watch, and what counts as a "new detection") and optionally the **action** (what to do on detection).

**When the argument is empty, there is no default watch target.** Always ask the user "what should I watch?" and only start the loop after receiving the answer as `<watch>`.

## Core principles

This Skill owns **only control (watching and dispatch)**. It does not hold the heavy context of implementation or investigation.

1. **Keep the main context lean.** The actual work of a picked-up task completes entirely inside a subagent; the main only receives a structured summary.
2. **One detection = one subagent.** Never reuse an existing subagent.
3. **Triage-first.** On detection, first dispatch a lightweight triage; only items judged needs-implement escalate to heavy implementation (see "Action ladder").
4. **Worktree is delegated to devflow.** Do not attach the harness's `isolation: "worktree"` on the sentinel side. Launch with `run_in_background: true` only, and let devflow's `preparing-worktrees` (persistent `.wt/<branch>`) own the worktree. Actions that don't write code need no worktree at all.
5. **No irreversible operations.** Never merge, release, roll out to production, or make externally destructive changes. Stop implementation at "mergeable state (Draft PR)" and investigation at "report"; wait for user approval for the final operation.
6. **In-session loop.** Polling self-paces via `ScheduleWakeup`. Closing the session stops it.
7. **Persist state to a file.** Keep the ledger at `~/.matsuyoshi30/sentinel/<slug>.json` and restore from the file on both re-entry and session resume.

## Action ladder

On detection, always enter a light triage first. Heavy devflow only fires when implementation is genuinely required.

```
Tier 0  triage / investigate・classify   (every time・no worktree・cheap)
          └→ verdict: no-action / light-done / needs-implement

Tier 1  light-done                       (no worktree)
          comment / file Linear ticket / reply / report
          the triage subagent finishes it in place and returns

Tier 2  implement                        (worktree + devflow --auto)
          only when code implementation is needed; heavy work starts here
```

How it moves:

- Detection → dispatch one Tier0 subagent, lightweight, `run_in_background: true`
- Verdict **no-action** → record `no_action` in the ledger, one-line report. devflow does not run.
- Verdict **light-done** → finish it in place and return `light_done` (ledger `done`, no second dispatch)
- Verdict **needs-implement** → sentinel **auto-fires** Tier2 (devflow --auto) (worktree + background)

Each recipe carries a **ceiling Tier** (see "Recipes"). A recipe with ceiling Tier1 does not escalate to Tier2 even if triage returns needs-implement; it reports `blocked` (needs user judgment) instead.

### Tier2 = `devflow --auto` behavior (important)

`devflow --auto` auto-approves its own recommended choice at each gate, but at some **hard guardrails it escalates to a human and waits** (`devflow/reference/auto-mode.md`). A Tier2 subagent runs unattended in the background, so it would **hang** at any waiting gate. To prevent this, split behavior per gate:

Step 0.5 NOT_LIGHT (early check for heavy tasks) → **choose stay-auto and self-drive (true auto-fire).**

- The user's choice is "auto-fire." Even on NOT_LIGHT, do not stop; proceed stay-auto to Draft PR.
- This lets the bulk of real tickets (which tend to be NOT_LIGHT) run without a human. The final brake on heaviness is handled by the "gates that genuinely need judgment" below.

Any other hard guardrail → **halt devflow and return `status: blocked`:**

- Close DR (both options near-tied) / spike INSUFFICIENT twice / ABORTED_RETRY_LOOP / review max exceeded / vague DoD / mid-run AUTO_DRIFT detected.
- These are "recommendation not uniquely determined / not resolvable by auto-progression"; even under stay-auto, devflow itself waits for a human. The Tier2 subagent must **not fabricate an answer — halt devflow** here and return `blocked` with "gate name / devflow's recommended option + alternatives / the decision needed."

Effect:

- Light / medium tickets → pass Step 0.5 (LIGHT self-drives, NOT_LIGHT stays auto) and self-drive to Draft PR ✅
- Tickets with a genuine fork → `blocked` at the relevant gate → sentinel reports. Once the user returns a decision, the next round's ledger sweep (Phase 0.5) **injects the decision and re-dispatches**.

This keeps the auto-fire promise while preventing unattended-background hangs and relaying only the forks that genuinely need a human.

## Workflow

### Phase 0: Interpret the watch spec (once, right after launch)

**If the argument is empty, first ask the user "what should I watch?"** Treat the answer as `<watch>` before proceeding.

Parse the argument and assemble the **watch spec**:

- **Trigger / source** — what to observe, and how. If it matches a "recipe," use the canned form; otherwise interpret it in natural language. Verify the recipe's poll tool actually exists in this environment (see Recipes notes); if the required MCP tool is absent, tell the user and stop rather than looping on a tool that will never resolve.
- **Detection unit and unique key** — make each target's stable unique key (Linear identifier / PR number / Sentry issue ID / Slack ts, etc.) the ledger key.
- **New detection** — a unique key absent from the ledger counts as "new."
- **Action (recipe)** — the triage and ceiling Tier on detection. If the argument explicitly states an action, that takes precedence.

Only confirm with the user minimally when critically ambiguous (e.g., team・repository could be several). **Present the finalized watch spec (trigger / unique key / recipe / ceiling Tier / report channel) in 1–2 lines before starting the loop.**

**Open the state file.** Derive a short `<slug>` from the watch spec and make `~/.matsuyoshi30/sentinel/<slug>.json` the state file. If the directory is missing, `mkdir -p ~/.matsuyoshi30/sentinel`.

- If the file exists, read it and restore the ledger.
- **First-run seed guard.** When the state file is new (= first launch), mass-dispatching all currently-matching existing targets as "new" causes an accident (10 Linear Todos → 10 devflow runs). On first run, record existing matches as **`seen` only, without dispatching**. Flush the backlog only when the user explicitly says "process the existing ones too." Report in one line how many were seeded as `seen`.
- **On a cold session resume, do not auto-restart restored in-flight items.** Closing and reopening a session kills its background subagents, so `triaging` / `implementing` items are orphans (their subagent is gone). A `blocked` item is *not* an orphan — its subagent already returned with the block — but it may still hold a leftover worktree, so its eventual re-dispatch needs the same worktree care. (All of this applies *only* to a cold resume where the session was closed and subagents are gone. On a normal `ScheduleWakeup` re-entry within the same live session, in-flight items are genuinely still running — do not treat them as orphans; just continue from the ledger.)
  - `triaging` / `pending_implement` orphans are cheap → fine to re-dispatch (no worktree, no artifact).
  - `implementing` and **`blocked`** orphans may have a leftover worktree (`.wt/<branch>`) / branch / partial commits / draft PR → **do not re-dispatch on your own**. Investigate the real state (PR present? partial commits? CI status?) and present it; let the user pick "continue / discard / mark done." If "continue" is chosen, follow "worktree discard note" below.
- If the file is missing, create it and write the finalized watch spec.
- Tell the user the path in one line.

### Phase 0.5: Ledger sweep (run every round, before new detection)

Phase 1 only picks up "new keys absent from the ledger." But items **still needing action** remain inside the ledger. Before new detection, walk the ledger once and handle these first (left alone, they are never revisited and die):

- **`blocked` items whose user decision has arrived** → re-dispatch the appropriate Tier with the decision injected, **subject to the same Tier2 slot check as `pending_implement`** (below). For Tier2, fill the implementation prompt's `[devflow gate answer]` slot with "which gate, what to choose" (e.g., "for the close DR, choose A"). Leave items with no decision yet untouched. **Worktree discard note**: devflow does not resume — it restarts from Step 0, so a leftover `.wt/<branch>` from the prior run collides with worktree creation at Step 4. Before re-dispatch, discard that worktree and branch (`git worktree remove --force .wt/<branch>` → `git branch -D <branch>`). The partial commits are lost, but since devflow cannot resume they would not be reused anyway (the gate answer steers the fresh re-run to the right fork). Add one line about the discard to the report.
- **`pending_implement` (needs-implement carried over by maxInFlight)** → fire Tier2 only if a slot is free (per the budget arbitration below). Otherwise leave it `pending_implement` for the next round.

**Tier2 budget arbitration (strict cap).** Both decided-`blocked` re-dispatches and `pending_implement` items compete for free Tier2 slots, where free = `implementing` count < `maxInFlight`. Fill slots in priority order — **decided-`blocked` first** (a human already waited on them; finishing reduces WIP), then `pending_implement` — until the cap is reached. Items that don't get a slot stay as-is (`blocked` keeps its `decision` field; `pending_implement` stays parked) and are retried next round. **Never exceed `maxInFlight`, even for decided items.**

A user decision can arrive via free text through the report channel / as an argument on the next `/sentinel` launch / as a direct in-session instruction. Write the received decision onto the relevant ledger item (`decision` field), then consume it in the sweep.

### Phase 1: Polling (new detection)

- Observe per the watch spec's trigger and fetch candidates meeting the condition.
- Match each candidate's unique key against the ledger and extract **only the new ones not yet dispatched**.
- If new count is 0 (and Phase 0.5 had no movement either), do nothing and go to Phase 4.

### Phase 2: Dispatch

For each new detection, launch a **Tier0 (triage) subagent**:

- `subagent_type: "claude"`
- `run_in_background: true`
- No isolation (triage writes no code)
- `description`: `"sentinel triage: <unique key>"`
- `prompt`: the triage prompt (template below)

On launch, **immediately write `triaging` to the ledger** (double-launch prevention). The main proceeds without waiting for completion.

#### Concurrency cap

Full `devflow --auto` is heavy. Many running in parallel in the background melt both tokens and CPU.

- Apply `maxInFlight` (default 3) to **Tier2 (`implementing`)**. Record the overflow as **`pending_implement`** in the ledger (prevents the never-revisited accident), and fire it when a slot opens in the next round's Phase 0.5.
- Tier0/Tier1 are cheap, so a separate budget (default 5) is fine.

#### Triage completion → escalation decision

On the triage subagent's return value (schema below):

- `no_action` → ledger `no_action`, one-line report.
- `light_done` (light action finished in place) → ledger `done`, report with artifact.
- `needs_implement` → if the recipe ceiling is Tier2: **auto-fire** if a Tier2 slot is free (below), else record `pending_implement` in the ledger (fired in the next round's Phase 0.5). If the ceiling is Tier1, the main converts the triage's `needs_implement` into ledger status `blocked` and reports it as "implementation needed but ceiling recipe — needs judgment" (the Tier1 triage subagent itself only ever returns `needs_implement`, never sets `blocked`).

#### Tier2 (devflow --auto) dispatch

- `subagent_type: "claude"`
- `run_in_background: true`
- No isolation (devflow creates a persistent `.wt/` via preparing-worktrees)
- `description`: `"sentinel implement: <unique key>"`
- `prompt`: the implementation prompt (template below, including the devflow hard-guardrail → blocked directive)

On launch, update the relevant ledger key to `implementing`.

### Phase 3: Aggregate completion・report

- On a subagent completion notification, take in only the result summary and update the ledger to `no_action` / `done` / `ready_to_merge` / `blocked` / `failed` (also write artifact・summary・updatedAt).
- Report **once per item, concisely** to the report channel (below). E.g.: `EMRK-123: mergeable ✅ PR #456` / `Sentry ABC: investigated・no action` / `EMRK-200: halted on close DR — A(…)/B(…) which?`
- For items right before an irreversible operation (e.g. `ready_to_merge`), **do not perform that operation**; wait for user instruction on the final step.

### Phase 4: Schedule next round / stop

- Call `ScheduleWakeup` with `prompt` set to the **full original launch command** (`/sentinel <watch>`) to re-enter. Decide `delaySeconds` yourself:
  - in-flight active / waiting on CI → short (within 270s to keep the cache warm)
  - no new items for a while・all idle → long (1200–1800s)
  - if event-driven and slow to change, match that change frequency
- **Stop** (do not call `ScheduleWakeup`) on any of:
  - The user said "stop."
  - Repeated failures of the same kind (do not retry infinitely; report and ask for instructions).

## Recipes

If the watch spec matches a recipe, use the canned form; otherwise interpret in natural language. Each recipe carries its poll means / unique key / Tier0 triage angle / ceiling Tier.

| Recipe | poll | unique key | triage angle | ceiling Tier |
|--------|------|------------|--------------|--------------|
| linear-implement | Linear MCP (`Linear:list_issues` / `Linear:get_issue`; identify assignee=self via `Linear:list_users`) (※ see note) | identifier (e.g. EMRK-123) | acceptance criteria present? / implementation needed? / already resolved? | Tier2 |
| github-pr-review | `gh` / `gh api` (PR list・review state・`gh pr checks`) | PR number | diff that needs review? / CI failure cause obvious? | Tier1 (review comments only; does not implement) |
| sentry-triage | `sentry-cli` / Sentry MCP `Sentry:*` (※ see note) | issue ID | known or new? / blast radius / action needed? / file a ticket? | Tier1 (investigate→file Linear ticket if needed; Tier2 only when user permits) |
| slack-mention | Slack MCP `Slack:*` search tool (※ see note) | message ts | reply/action needed? / kind of content | Tier0/1 (depends on content) |

Notes:

- **MCP tool names are shown fully qualified as `Server:tool`** (per Skill best practice, to avoid "tool not found"). The server prefix is environment-specific — adjust `Linear:` / `Sentry:` / `Slack:` to whatever your session actually exposes (e.g. a `mcp__…__list_issues` form). Confirm the real names with a tool search before relying on a recipe.
- **The Linear / Sentry / Slack MCP tools are environment-dependent.** This skill bundles none of them. `linear-implement` needs a Linear MCP, `sentry-triage` needs `sentry-cli` or a Sentry MCP, `slack-mention` needs a Slack MCP. Before starting a recipe, verify its tool is present; if absent, tell the user that recipe is unusable. (Only `github-pr-review`, which uses the `gh` CLI, has no MCP dependency.)
- An explicit action in the argument overrides the recipe default.

#### linear-implement default implementation action (Tier2)

When triage returns `needs_implement` and Tier2 fires, the action handed to the subagent:

```
1. Implement <target ticket> with the devflow --auto skill
   (fetch the ticket body・acceptance criteria from Linear → explore → plan → spike → impl → review → verify).
   devflow's preparing-worktrees creates the persistent .wt/ worktree (leave it to this subagent).
2. When devflow reaches Draft PR, return ready_to_merge. PR title in Japanese, linked to the Linear ticket.
3. For devflow's Step 0.5 NOT_LIGHT, choose stay-auto and self-drive (do not stop).
   For any other hard guardrail (close DR / spike INSUFFICIENT twice / ABORTED_RETRY_LOOP /
   review max exceeded / vague DoD / AUTO_DRIFT), do not fabricate an answer — halt devflow and
   return status: blocked.
4. Goal = mergeable state. Never merge.
```

## Prompt templates handed to subagents

### Tier0 (triage)

```
You are triaging the following target detected by sentinel.

[Target]
<unique key, plus a summary of the target (ticket summary / PR title / error content, etc.)>

[Recipe]
<recipe name, plus its triage angle / ceiling Tier>

[What to do]
1. Investigate the current state only as far as needed (do not do heavy implementation).
2. Classify into one of:
   - no_action  : literally nothing remains to do (false positive / fine to wait / already resolved AND no cleanup left). Put the reason in summary.
   - light_done : a light action (comment / close stale ticket / reply / report) completes it in place → do it and return the artifact.
     Tie-breaker vs no_action: if the item is already resolved but a small administrative cleanup remains (e.g. closing a stale ticket, leaving a "fixed in #88" comment), that cleanup is `light_done`, not `no_action` — a ticket close/comment is reversible and is NOT in the irreversible list (merge / release / production rollout / externally destructive change), so it is allowed here.
   - needs_implement : code implementation is needed. Do not implement; put what should be implemented in summary.
3. If a ceiling-Tier1 recipe reaches needs_implement, do not implement — return needs_implement
   (sentinel will treat it as blocked).

[Common rules]
- Do not perform irreversible operations (merge / release / production rollout / externally destructive changes).
- Do not do heavy implementation or large-scale changes here (that is Tier2's job).

Return the final message structured (no human-facing decoration):
- key: <unique key>
- verdict: no_action | light_done | needs_implement
- artifact: <for light_done, the created comment/ticket URL etc. "none" if absent>
- summary: judgment and rationale in 1–3 lines
- blockers: any critical gap you are unsure about (empty if none)
```

### Tier2 (implement / devflow --auto)

```
You are handling the following target that sentinel judged "needs implementation."

[Target]
<unique key and summary, plus the triage summary>

[Action to perform]
<expand the linear-implement default implementation action, or the action specified in the argument>

[devflow gate answer]
<re-dispatch only: the gate that was blocked last time and the decision the user returned.
 e.g. "for the close DR (A: … / B: …), choose A and continue." Empty if none>

[Common rules]
- Do not perform destructive・irreversible operations (merge / release / production rollout). Stop at Draft PR (mergeable state).
- This worktree is the isolated environment devflow's preparing-worktrees creates.
- For devflow's Step 0.5 NOT_LIGHT, choose stay-auto and self-drive (do not stop).
- For any other hard guardrail (close DR / spike INSUFFICIENT twice / ABORTED_RETRY_LOOP /
  review max exceeded / vague DoD / AUTO_DRIFT), do not proceed on your own — halt devflow and
  return status: blocked (state gate name / recommended option + alternatives / the decision needed).
  However, if [devflow gate answer] gives a decision for that gate, follow it and continue.

Return the final message structured:
- key: <unique key>
- status: ready_to_merge | blocked | failed
- artifact: <PR URL / created ticket etc. "none" if absent>
- summary: what you did in 1–3 lines
- blockers: reason for halting・decision needed (empty if none)
```

## Report channel

The destination is a **config point**. Include it in the watch spec for the user to choose. Default is inline. Use only what is available; fall back to inline when absent (do not hardcode).

- `inline` (default) — report as the main's response. Assumes the session is being watched.
- `slack` — DM / mention via an available Slack tool. For environments with a Slack path like masanyang.
- `discord` — post via the Discord MCP reply (push notification). Alternative when Slack is absent but this is available.
- `memory` — append processing history thinly to `~/.matsuyoshi30/memory` (traceable later).

Multiple allowed (e.g. `inline` + `slack`). For `blocked` (needs judgment), prefer a push-notifying channel (slack / discord) so you notice while away.

The same resolved channel serves both layers: sentinel's own per-item status reports (Phase 3) **and** a Tier1 light-done action whose action is itself "report to X" (e.g. "investigate and report to Slack"). Don't build a separate path for the two — resolve the channel once and use it for both, applying the same absent→inline fallback.

## State file

Persist each target's processing state to `~/.matsuyoshi30/sentinel/<slug>.json`. `<slug>` is a stable key derived from the watch spec (the same watch content lands on the same file every time).

Schema:

```json
{
  "slug": "assigned-linear-todo",
  "watchSpec": {
    "raw": "自分にアサインされた Linear の Todo チケットを実装して",
    "source": "linear",
    "recipe": "linear-implement",
    "trigger": "assignee = self, state = Todo",
    "keyField": "Linear identifier (e.g. EMRK-123)",
    "ceilingTier": 2,
    "report": ["inline", "slack"]
  },
  "startedAt": "2026-06-13T03:00:00Z",
  "updatedAt": "2026-06-13T03:34:00Z",
  "maxInFlight": 3,
  "items": {
    "EMRK-123": {
      "status": "ready_to_merge",
      "artifact": "https://github.com/.../pull/456",
      "summary": "implemented …, CI green",
      "blockers": "",
      "dispatchedAt": "2026-06-13T03:05:00Z",
      "updatedAt": "2026-06-13T03:30:00Z"
    }
  }
}
```

- `items` keys are the unique key.
- Allowed `status` values: `seen` (first-run seed) / `triaging` / `pending_implement` (needs-implement, waiting for a slot) / `implementing` / `no_action` / `done` / `ready_to_merge` / `blocked` / `failed`.
  - `triaging` / `pending_implement` orphan = safe to re-dispatch (no worktree, no artifact). `implementing` / `blocked` orphan = may have a worktree/PR, handle carefully (see Phase 0).
  - When a user decision is received for a `blocked` item, write a `decision` field. Phase 0.5 injects it into Tier2's `[devflow gate answer]` and re-dispatches.
  - `seen` items only need `status` + `updatedAt` (and optionally a one-line `summary`); the other fields (`artifact`, `blockers`, `dispatchedAt`) may be omitted or empty since nothing was dispatched.
- Write whenever state changes (on dispatch・on completion notification). Only the main reads/writes; subagents do not touch it (the main reflects via the return-value summary).
- Timestamps via `date -u +%Y-%m-%dT%H:%M:%SZ`.
- Thanks to this file, re-running `/sentinel <same watch content>` in another session does not double-launch already-processed targets.

## Safety devices

- **The triage gate caps cost.** Don't run heavy devflow on every detection; first judge action-need with cheap Tier0.
- **First-run seed guard.** Don't mass-implement the backlog on first launch (existing ones are `seen`-recorded only).
- **Concurrency cap.** `maxInFlight` (default 3) limits parallel Tier2.
- **Double-launch prevention.** Persist to the ledger at the moment of dispatch so no two subagents stand up for the same target (even across sessions).
- **Relay devflow hard guardrails as blocked.** Don't let unattended-background gates hang.
- **No irreversible operations.** Stop merge・release・production rollout just before the goal and wait for approval.
- **No infinite error loops.** Retries are finite. When stuck, stop and report.
- **Protect the main context.** Don't pull implementation logs・diffs・large output into the main. Hold only the structured summary.
- **Beware double-driving with masanyang.** sentinel's and masanyang's ledgers don't see each other. Pointing both at the same Linear queue (same assignee/state) can produce two worktrees and two PRs for the same ticket. Don't point both at the same queue.

## Launch examples

```
/sentinel                                          # no arg → asks "what should I watch?" (no default behavior)
/sentinel 自分にアサインされた Linear の Todo チケットを実装して       # linear-implement, ceiling Tier2
/sentinel henry-web に自分宛てレビュー依頼が来たらレビューしてコメント   # github-pr-review, ceiling Tier1
/sentinel Sentry の production に新規エラーが出たら調査して Linear 起票  # sentry-triage, ceiling Tier1
/sentinel EMRK-1234 にサブチケットが追加されたら実装して               # natural-language fallback
```
