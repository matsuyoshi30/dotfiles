# pi (pi.dev) configuration

Harness configuration for @earendil-works/pi-coding-agent. install.sh
symlinks each item below into ~/.pi/agent/ individually.

# Layout

- agent/settings.json global settings (default provider, etc.)
- agent/models.json custom provider definitions (local llama.cpp, etc.)
- agent/prompts/ prompt templates (expanded via /name)
- agent/extensions/ TypeScript extensions (notify, dangerous-command gate, protected paths)
- Global instructions come from .agents/AGENTS.md, linked to ~/.pi/agent/AGENTS.md
- Skills need no extra setup: pi reads ~/.agents/skills/ natively

Runtime files (auth.json / trust.json / sessions/) stay as real files under
~/.pi/agent/ and are never put in this repo. sessions/ may contain customer
data.

# Extensions inventory

Self-written (designed for this setup):

- notify.ts plays Submarine.aiff and sends an OSC 777 terminal notification
  when the agent finishes its turn; silent in print/RPC mode
- permission-gate.ts confirmation prompt before destructive bash commands
  (rm -rf, sudo, chmod/chown 777, git push --force, git reset --hard,
  git clean -f), naming the matched rule; refuses outright when no UI
- protected-paths.ts refuses write/edit on .env, .env.*, .git, .ssh,
  node_modules; matches whole path segments to avoid false positives
- add-dir.ts /add-dir, /remove-dir, /dirs commands: appends external dirs'
  AGENTS.md/CLAUDE.md to the system prompt each turn. Dir list persists in
  the session. Replacement for the community pi-add-dir package
  (context files only; does not load skills from added dirs)
- todo.ts todo tool (add/done/remove/clear/list) + /todos viewer; each tool
  result carries the full list, rebuilt by replaying the branch so it
  follows /resume and /tree. Replacement for @juicesharp/rpiv-todo
- ask-user-question.ts question tool: the model asks, the user picks from
  options or types a custom answer; uses pi's stock select/input dialogs.
  Replacement for @juicesharp/rpiv-ask-user-question

Prompt templates: prompts/review.md (/review staged diff review),
prompts/commit.md (/commit draft a commit message, propose splits).

# Installed pi packages (pinned)

Declared in agent/settings.json packages; pi 0.74.2 installs them via
npm -g into /opt/homebrew/lib/node_modules. Pinned specs are skipped by
pi update; to move a version, review the diff first, then
pi install npm:<name>@<new-version>.

- npm:pi-subagents@0.33.1 (nicobailon/pi-subagents) subagent orchestration.
  Audited at install: no lifecycle scripts; deps are official pi-tui, jiti,
  typebox
- npm:pi-observational-memory@3.0.3 (elpapi42/pi-observational-memory)
  tiered compaction with observations/reflections. Audited at install: no
  lifecycle scripts, no runtime deps of its own (node_modules bulk is the
  official @earendil-works peer-dependency stack)

# Supply chain policy

- Official means the @earendil-works npm scope (GitHub org earendil-works)
  only. A pi-* npm name or a pi.dev/packages listing says nothing about
  trust; anyone can publish there
- Small capabilities (roughly one extension file): do not install packages.
  Port them by hand from examples/extensions/ in the official repo and keep
  all executable code tracked in this repo. Extensions run with full system
  permissions
- All extensions here are self-written; none carry upstream code. If
  example code is ever vendored or closely adapted again, keep the upstream
  MIT copyright notice in the file header and ship the license text next to
  it (this repo is public, so vendored files are redistribution)
- Large capabilities where a hand port is not worth maintaining: allowed as
  an exception with an exact pinned version, after checking the package for
  lifecycle scripts and its dependency list. Record the audit in this file
  (see Installed pi packages above)
- Extensions must load zero third-party code at runtime. node_modules exists
  only for editor type support (devDependencies only, installed with
  --ignore-scripts, gitignored). Extensions reference it via import type
  only, which is erased at runtime, so none of that code runs under pi
- Never write literal API keys in models.json. Use pi's value resolution
  ("$ENV_VAR" interpolation / "!command" execution) instead

# Verifying extensions

cd .pi/agent/extensions && npm install --ignore-scripts && npx tsc -p .

Types are checked against pi's own definitions (including event names and
return shapes). For behavior, a quick smoke test is
node_modules/.bin/pi -p --no-session "..."
