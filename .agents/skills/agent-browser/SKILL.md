---
name: agent-browser
description: Scripted browser automation via the `agent-browser` CLI — accessibility-tree `@ref` selection, semantic locators, isolated parallel sessions, Chrome profile reuse, CDP connect, and measurement (Core Web Vitals, HAR, traces, React re-render profiling, visual diff). Use when the browser work should run without disturbing the user's own Chrome: unattended or parallel runs, repeated scripted flows, and performance or diagnostic measurement. When the user simply wants a logged-in page in their own browser read or driven, use claude-in-chrome instead.
allowed-tools: Bash(agent-browser:*)
---

# Browser automation with agent-browser

A daemon-backed CLI. The browser stays open between commands, so chaining with `&&`
in one shell call is both safe and fast.

## This skill or claude-in-chrome

Reach for claude-in-chrome when the user's own browser is the point — reading or
driving a page they are already logged into, in the session in front of them.

Reach for `agent-browser` when the user's Chrome should stay untouched:

- unattended runs, or several independent browsers at once
- a scripted flow repeated against a dev server or a staging build
- measurement and diagnostics (Web Vitals, HAR, traces, React re-renders, visual diff)

## Load the CLI's own guide first

```bash
agent-browser skills get core --full
```

The guide ships inside the CLI, so it is always version-matched — prefer it over
guessing commands from flags. Specialized guides exist for other targets:

```bash
agent-browser skills list
agent-browser skills get electron   # VS Code, Slack, Discord, Figma desktop apps
agent-browser skills get slack
agent-browser skills get dogfood    # exploratory bug hunting
```

## Just the rendered text

When the page only needs to be read — an SPA-rendered docs page that a plain fetch
returns empty, for instance — skip the interaction loop entirely:

```bash
agent-browser read <url>            # agent-readable text of the rendered page
agent-browser get html --selector main
```

## Core loop

Snapshot, act on the refs it returns, re-snapshot after navigation or a large DOM change.

```bash
agent-browser open https://example.com/login
agent-browser snapshot -i          # interactive elements only → @e1, @e2, @e3
agent-browser fill @e1 "user@example.com"
agent-browser fill @e2 "password"
agent-browser click @e3
agent-browser wait --load networkidle
agent-browser snapshot -i          # confirm the result
agent-browser close
```

Semantic locators work without a snapshot when the target is unambiguous:

```bash
agent-browser find role button click --name "Submit"
agent-browser find label "Email" fill "user@test.com"
```

Add `--json` to any command for machine-readable output.

## Reaching authenticated pages

Four routes, ordered by how little they disturb the user's browser. Prefer the first
that works.

```bash
# 1. Replay a saved cookie + storage bundle — the default for repeatable runs
agent-browser state save auth.json      # after logging in once
agent-browser --state auth.json open https://app.example.com/dashboard

# 2. Auto-save and restore per session, with a staleness check
agent-browser --restore myapp --restore-check-text "Sign out" open https://app.example.com

# 3. Reuse a real Chrome profile's existing logins
agent-browser profiles                  # list them, then ask the user which to use
agent-browser --profile Default open https://github.com

# 4. Attach to a Chrome already running with remote debugging
agent-browser --auto-connect open https://example.com
```

`--auto-connect` also converts an existing login into a reusable bundle:
`agent-browser --auto-connect state save ./auth.json`.

For a site that demands a fresh form login every time, `agent-browser auth save <name>`
stores the credentials and `agent-browser auth login <name>` replays them.

## Parallel sessions

Each named session is an isolated browser with its own cookies and storage.

```bash
agent-browser --session a open https://site-a.com
agent-browser --session b open https://site-b.com
agent-browser session list
agent-browser close --all
```

## Measurement and diagnostics

```bash
agent-browser vitals --json                      # LCP/CLS/TTFB/FCP/INP + hydration
agent-browser network har start ./trace.har      # ... then: network har stop
agent-browser network requests --filter /api/
agent-browser trace start                        # ... then: trace stop ./trace.json
agent-browser diff screenshot --baseline         # visual regression against a baseline
agent-browser console                            # console logs
agent-browser errors                             # page errors
```

React internals need the flag at launch:

```bash
agent-browser open http://localhost:3000 --enable react-devtools
agent-browser react renders start
# ... interact ...
agent-browser react renders stop --json
agent-browser react suspense --only-dynamic
```

## Debugging a flow that misbehaves

```bash
agent-browser open <url> --headed   # watch it happen
agent-browser highlight <selector>  # confirm you are targeting what you think
agent-browser inspect               # open DevTools on the active page
agent-browser doctor --fix          # install and stale-state problems
```
