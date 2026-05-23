---
name: grasp-check
description: Assess your own grasp of a topic through a structured dialogue, then output a learning roadmap. Runs in three stages — multiple-choice weakness mapping, Socratic deep-dive on weak areas, and a saved Markdown learning guide. Use when the user wants to check how well they understand a topic ("how well do I understand X?", "grill me on X", "self-assess my knowledge of X"), or when they want to objectify their grasp before / after self-study. Not for brainstorming ideas (use shaping-spec) or for stress-testing a plan (use grill-me-style design review).
allowed-tools: Read, Write, Glob, Grep, Bash, Skill(gemini-search), WebSearch
user-invocable: true
---

# Grasp Check

Help the user measure their understanding of a topic, locate the weak spots, and hand back a concrete learning roadmap. The output is a Markdown report saved to the notes directory; the value is not "you scored 7/10" but "here are the three things you actually don't understand and what to read next."

**Announce at start:** "Using grasp-check skill to assess your grasp on `{topic}`."

## Positioning vs Adjacent Skills

- **shaping-spec / shaping-screens** — organize your *ideas* (outward-facing). Use when you have a vague thought to turn into a spec.
- **grasp-check (this skill)** — organize your *knowledge* (inward-facing). Use when you want to know what you don't know.
- **gemini-search** — invoked from inside grasp-check to fetch concrete learning resources; not a substitute for this skill.

## Modes

The skill operates in one of two modes, decided in Stage 0.

- **B mode (default)**: any topic. Question generation and evaluation use the LLM's knowledge. Resource recommendation uses `gemini-search` (with `WebSearch` as fallback).
- **C mode (repo-bound)**: the topic ties to the current repository. Questions may reference real code via Grep/Glob/Read, and the learning guide includes `file:line` references.

C mode is triggered when the topic contains repo-bound signals:

- Possessive / proximal markers: "our", "this repo", "うち", "この" + a noun
- Names that match top-level directory or notable file names in the current repo
- Explicit user statement that the topic is repo-specific

When any signal fires, ask **once**: "Treat this as bound to the current repo (C mode)? Y/N." Default to B mode if the user says no or the signal is weak.

## Flow

Register this checklist with TodoWrite at the start and tick items off as you go. If `TodoWrite` is not available in the current environment, write the equivalent checklist verbatim at the top of your first user-facing message and tick items off in subsequent messages — the visible progress matters, the tool does not.

```
grasp-check progress:
- [ ] Stage 0: Topic intake, mode decision, breadth estimate
- [ ] Stage 1: Weakness mapping (N multiple-choice questions, no feedback)
- [ ] Gate: User chooses to deep-dive or stop
- [ ] Stage 2: Socratic deep-dive on weak topics (optional)
- [ ] Stage 3: Generate learning guide and save to notes directory
```

### Stage 0: Intake

1. If the user invoked the skill without a topic, ask exactly one question: "Which topic do you want to assess?"
2. Decide the mode (B or C) using the signals above. If C-mode signals fire, ask once for confirmation.
3. Estimate the topic's breadth — how many major sub-concepts does it contain?
   - Let `K` = your estimate of major sub-concepts. **Granularity rule**: count only "pillars a learner must be able to explain independently". Do not count finer sub-divisions under each pillar. Example for "HTTP caching": `Cache-Control`, `Expires`, conditional requests (`ETag` / `If-None-Match`), `Vary`, freshness model, shared vs private caches → K ≈ 6. Do not break `Cache-Control` into individual directives.
   - Set `N` (Stage 1 question count) = `clamp(round_half_up(K * 1.25), 5, 10)`. `round_half_up` means 7.5 → 8, 6.5 → 7.
4. If **either** `K >= 15` **or** the topic is clearly multi-layered (e.g. "programming", "AI", "distributed systems") — these two conditions are an **OR**, satisfying one is enough — do not start Stage 1. Instead, propose 3–5 narrower sub-topics and ask the user to pick one. Re-run Stage 0 with the chosen sub-topic.

### Stage 1: Weakness Mapping (multiple choice)

Output `N` questions, **one at a time**, in this exact shape:

```
Q{i}/{N}: {question text}
A. {option A}
B. {option B}
C. {option C}
D. {option D}
E. I don't know
```

Requirements:

- Each distractor (B/C/D when A is correct, etc.) must encode a **specific common misconception**, not a random wrong answer. The misconception is what the skill will later report as the weakness.
- Do **not** give feedback after each answer. Just say "Next." and move to the next question.
- Internally track, for every question: the concept tag, the correct option, the user's choice, and "if they chose X, which misconception does that imply".
- If the user picks E on 40%+ of questions, stop Stage 1 early. Tell the user: "The topic looks largely unfamiliar; skipping the deep-dive and going straight to an entry-level learning guide." Then jump to Stage 3.

After question `N`, print a scorecard:

```
Score: {correct}/{N}

Weak topics:
- T1: {sub-topic name} — missed on Q{a}, Q{b} → suggests misconception "{label}"
- T2: ...
```

A "weak topic" is any sub-topic where the user got at least one question wrong or picked E. If everything was correct, congratulate the user and skip to Stage 3 with a brief "next-level" guide.

### Gate: Deep-Dive or Stop

Ask: "Continue to deep-dive on these weak topics, or stop here and generate the learning guide?"

- "Continue" → Stage 2
- "Stop" → Stage 3

### Stage 2: Socratic Deep-Dive (open-ended)

For each weak topic `Ti`, ask open-ended questions one at a time. Templates:

- "In your own words, explain `{Ti}`."
- "Why does `{Ti}` exist — what problem does it solve?"
- "How does `{Ti}` differ from `{related concept}`?"
- (C mode only) "Where in this repo is `{Ti}` implemented or used?"

For each answer, judge it as one of:

- **Correct**: all main elements present, no errors → move to the next `Ti`.
- **Partial**: some elements missing or minor terminology error → say what is right and what is missing, ask the user to try again.
- **Wrong**: fundamental misconception → offer a counterexample ("if your model were right, X would happen — but X doesn't, why?"), then ask from a different angle.

Hard limit: **at most 3 questions per `Ti`**. If the user has not converged after 3, mark `Ti` as "needs focused study" and move on. Do not loop forever.

**`deep-dived` vs `needs focused study` decision rule** (used in the Stage 3 Status field):

- `deep-dived` requires **either**: (a) the user's final answer for this `Ti` was judged **Correct**, **or** (b) at least **2 open-ended answers** were judged and the misconception was identifiable enough to write a concrete "Self-check question" in the report.
- Otherwise — including the cases "only 1 partial/wrong answer before moving on", "user said 'enough' before judging this Ti at all", and "3-question cap hit without convergence" — mark as `needs focused study`.

When every `Ti` has been visited once, ask: "Anything else you want to dig into?" If yes, continue; otherwise, Stage 3.

### Stage 3: Learning Guide

Build the report by selecting a resource per weak topic:

- **C-mode topic** → identify the relevant code with Grep/Glob/Read and cite it as `path:line`. `gemini-search` is **optional** here — call it only when a general-concept refresher would clearly help the user (the code alone doesn't teach the underlying principle). Pure code citations are a valid output.
- **B-mode, concrete tech** (specific library / API / system) → call `gemini-search` for canonical docs and a high-quality article. Use real URLs returned by the search.
- **B-mode, classical concept** (well-established CS / engineering topics) → cite from your own knowledge: canonical book title, official spec name, or a precise search keyword. Do not invent URLs. `gemini-search` is optional; skip it when the canonical sources are well-known to you.

**gemini-search usage rules**:

- Invoke via the `Skill` tool (`Skill(gemini-search)`). The skill internally runs the `gemini` CLI via Bash — this is expected; do not separately invoke Bash yourself.
- **Budget: at most 1 call per weak topic** and at most 3 calls per whole report. Latency is high (often 30s+); pacing matters.
- If a `Skill(gemini-search)` call returns an error or instructs you to call Bash directly, treat that as a search failure for this topic and apply the fallback chain below.

**Fallback chain** when `gemini-search` cannot return real URLs:

1. Try `WebSearch` once. If it's a deferred tool, load it via `ToolSearch` first (`select:WebSearch`), then call it.
2. If `WebSearch` also fails (tool-level error, no usable result, or unavailable in this environment), fall back to LLM knowledge: book titles, official spec names, precise search keywords — never invented URLs.
3. On the LLM-knowledge fallback, add a note on the affected topic: "Resources generated from LLM knowledge only — verify before using." This note belongs on the topic where search failed, not as a blanket disclaimer.

Write the report to:

```
{base_dir}/grasp-check/{YYYY-MM-DD}_{topic-slug}.md
```

`{base_dir}` resolves in this fallback order (stop at the first writable one):

1. `{project_root}/.matsuyoshi30/`
2. `{project_root}/.matsuyoshi/`
3. `$HOME/.matsuyoshi30/`
4. `$HOME/.matsuyoshi/`

`{project_root}` is the current git top level (`git rev-parse --show-toplevel`), or the cwd if not in a git repo. `{topic-slug}` is a short kebab-case slug derived from the topic (max ~50 chars). If the target file already exists, append `_{HH-MM}` to the slug.

If all four locations fail to write, print the full Markdown report to the terminal and tell the user: "Could not save; please copy the report above."

#### Self-Verification Before Saving

Before writing the report file, run through this checklist on the draft. Fix anything that fails, do not skip.

- [ ] Every weak topic in "Weakness Breakdown" has at least one entry under "Next to learn".
- [ ] Every entry under "Next to learn" is one of: a URL returned by `gemini-search` / `WebSearch` in this session, a real `path:line` reference verified via Read, a canonical book / spec name you are confident exists, or an explicit search keyword. **No invented URLs.** If you cannot back a URL with a recent tool call, replace it with a keyword or book title.
- [ ] Every C-mode `path:line` citation has been confirmed by reading the file in this session.
- [ ] Every weak topic has a concrete "Self-check question" that maps to the observed misconception (not a generic "explain X" placeholder).
- [ ] The "Summary" section names the single biggest gap in one sentence, not a vague restatement of the score.
- [ ] If gemini-search / WebSearch failed for any topic, the report explicitly notes "Resources generated from LLM knowledge only — verify before using" on that topic.

Only after the checklist passes, call Write.

#### Report Template

```markdown
# Grasp Check: {topic}

- Date: {YYYY-MM-DD}
- Mode: {B | C}
- Score: {correct}/{N}
- Weak topics deep-dived: {M}

## Summary

{2–4 sentence overview of where the user stands and the biggest gap}

## Weakness Breakdown

### 1. {T1}

- **Status**: {`deep-dived` | `needs focused study` — apply the Stage 2 decision rule defined above (Correct final answer **or** ≥2 judged open answers → `deep-dived`; everything else → `needs focused study`)}
- **Observed misconception**: {what the wrong answers and deep-dive revealed; for `needs focused study` topics, summarise only what Stage 1 showed}
- **Next to learn**:
  - {Resource 1 — URL / book / path:line / keyword}
  - {Resource 2}
- **Self-check question** (use as the entry point next time):
  - {one concrete question that, if the user can answer it cleanly, means this weakness is resolved}

### 2. {T2}
...

## Next Steps

- {If gaps are large: suggest re-running grasp-check on the same topic after focused study}
- {If sub-topics surfaced: suggest spawning a grasp-check on the most critical sub-topic}
```

After writing the file, print only a short summary to the screen — the scorecard, the list of weak topics, and the saved file path. Tell the user to read the file for details. If the user has no immediate follow-up task related to the same topic, suggest `/clear` before moving to unrelated work, since deep-dive transcripts can be long.

## Edge Cases

- **E1. User says "I don't know" to most Stage 1 questions** — early-stop rule above (≥40% E answers → skip deep-dive, go to entry-level guide).
- **E2. User stays stuck on a `Ti` after 3 deep-dive questions** — mark as "needs focused study" and move on; the report will flag it.
- **E3. User abandons or stops early.** Behaviour depends on which stage:
  - **Stage 0 abandon** (before any Stage 1 question is asked): produce no report file. Just acknowledge and stop.
  - **Stage 1 abandon** (mid-questions, before scorecard): confirm "Pause grasp-check?" If yes, write a partial report with `_partial` appended to the slug.
  - **Stage 2 early stop** (user says "enough" / "no more topics" at any point during Stage 2, regardless of how many topics were actually deep-dived): treat as **normal completion**, not partial. Proceed to Stage 3 as usual. Any weak topic that does not meet the `deep-dived` rule above is marked `needs focused study` in the report (same handling as E2). Even if zero topics qualify as `deep-dived`, this is still normal completion — the scorecard from Stage 1 is the minimum useful output.
- **E4. Topic is too broad** — Stage 0 sub-topic split applies; do not start Stage 1.
- **E5. Both gemini-search and WebSearch fail** — the report is still produced; resources fall back to LLM knowledge and the report explicitly notes this.
- **E6. C mode selected but no relevant code found** — say "No matching code in this repo; switch to B mode or rephrase the topic?" and wait for the user.
- **E7. All save locations fail** — print full Markdown to terminal as a last-resort fallback.
- **E8. User asks a meta-question instead of answering** — explain the intent in 1–2 lines, do not change Stage 1 options (preserves scoring consistency), then ask the user to answer when ready.
- **E9. Re-running on the same topic on the same day** — append `_{HH-MM}` to the slug; do not overwrite or auto-compare against past reports.

## Non-Goals (YAGNI)

- No diffing or trend analysis against past reports.
- No recursive spawning of sub-grasp-checks; only *suggest* the next topic in the report.
- No persistent score database or dashboard.
- No "teach the topic from scratch" mode — grasp-check measures and points; it does not lecture.

## Anti-Patterns

- **Giving away the answer mid-Stage-1.** Stage 1 is mapping; immediate feedback contaminates the remaining questions in the same concept area.
- **Random distractors.** Every wrong option in Stage 1 must reflect a specific common misconception, otherwise the weakness signal is noise.
- **Looping forever on one weakness in Stage 2.** Hard cap at 3 questions per `Ti`; if unresolved, defer it to the learning guide.
- **Hallucinating URLs.** If you cannot get a real URL from `gemini-search` / `WebSearch`, cite by title / keyword instead. Do not fabricate links.
- **Producing only a screen summary.** The Markdown report is the deliverable; if you cannot save it, dump the full content so the user can save it manually.
