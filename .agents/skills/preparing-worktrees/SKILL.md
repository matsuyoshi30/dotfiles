---
name: preparing-worktrees
description: Use when setting up an isolated git worktree for feature work — selects the worktree directory with `.wt/` as the top priority, verifies `.gitignore` safety, creates the worktree on a new branch, and runs project setup. Pairs with devflow Step 4 Isolation gate.
---

# Preparing Worktrees

Creates an isolated, persistent git worktree so feature work does not touch the base branch's working tree. Survives across multiple agent dispatches (unlike the harness's `isolation: "worktree"` which is per-dispatch and disposable).

**Announce at start:** "Using preparing-worktrees skill to set up an isolated workspace."

## Directory Selection (priority order)

Pick the first match.

1. **`.wt/`** at repo root — preferred
2. **`.worktrees/`** at repo root
3. **`worktrees/`** at repo root
4. **`CLAUDE.md`** preference — `grep -i "worktree.*director" CLAUDE.md`
5. **Ask the user**:
   ```
   No worktree directory found. Where should I create worktrees?
   1. .wt/ (project-local, hidden, recommended)
   2. ~/.config/claude/worktrees/<project>/ (global)
   ```

If the chosen directory does not exist yet, create it.

## Safety: `.gitignore` Verification

For project-local directories (`.wt/`, `.worktrees/`, `worktrees/`), verify the directory is ignored before creating the worktree:

```bash
git check-ignore -q .wt 2>/dev/null
```

If NOT ignored:
1. Append the directory name (with trailing `/`) to `.gitignore`
2. Commit the `.gitignore` change
3. Proceed

Why: prevents accidentally committing worktree contents into the repo.

Global directories (`~/.config/claude/worktrees/...`) skip this check.

## Creation Steps

### 1. Resolve paths

```bash
repo_root=$(git rev-parse --show-toplevel)
project=$(basename "$repo_root")
```

Full worktree path:
- Project-local: `$repo_root/$LOCATION/$BRANCH_NAME`
- Global: `~/.config/claude/worktrees/$project/$BRANCH_NAME`

### 2. Create the worktree

```bash
git worktree add "$path" -b "$BRANCH_NAME"
```

If the branch already exists, drop `-b` and check out the existing branch instead.

### 3. Project setup (auto-detect)

Run whatever the project needs to be usable. Detect from files at the worktree root:

| File present | Run |
|--------------|-----|
| `package.json` + `package-lock.json` | `npm install` |
| `package.json` + `pnpm-lock.yaml` | `pnpm install` |
| `package.json` + `yarn.lock` | `yarn install` |
| `Cargo.toml` | `cargo build` |
| `pyproject.toml` (poetry) | `poetry install` |
| `requirements.txt` | `pip install -r requirements.txt` |
| `go.mod` | `go mod download` |
| `Gemfile` | `bundle install` |

If none match, skip.

### 4. Baseline check (optional)

Skip by default. Callers (e.g. devflow Step 7) already run verification at the right moment. Only run a baseline test when the caller explicitly asks for one — e.g. to distinguish pre-existing failures from changes introduced inside the worktree on long-running tasks.

### 5. Report

```
Worktree ready at <full-path>
Branch: <branch-name>
Setup: <command run, or "skipped">
```

Return both the absolute path and the branch name so the caller can `cd` in and pass the path to subsequent dispatches.

## Quick Reference

| Situation | Action |
|-----------|--------|
| `.wt/` exists | Use it (verify ignored) |
| `.wt/` absent, `.worktrees/` exists | Use `.worktrees/` (verify ignored) |
| `.wt/` and `.worktrees/` absent, `worktrees/` exists | Use `worktrees/` (verify ignored) |
| None exist, CLAUDE.md specifies a directory | Use it |
| None of the above | Ask user (`.wt/` vs global) |
| Directory not ignored | Append to `.gitignore`, commit, proceed |
| Branch already exists | `git worktree add <path> <branch>` (no `-b`) |
| No recognizable project files | Skip setup |

## Rules

- **Never** create a project-local worktree directory without confirming it is gitignored.
- **Never** skip the priority order — `.wt/` wins over `.worktrees/` even if both exist.
- **Never** commit inside the worktree on behalf of the caller — commits are the caller's responsibility.
- **Always** return the absolute worktree path and branch name to the caller.

## Integration

**Called by:**
- `devflow` Step 4 preamble — when the Isolation gate decides WORKTREE
- Any skill or workflow that needs a persistent isolated workspace

**Pairs with:**
- `devflow` Step 7 — verification runs inside the worktree
- Cleanup: `git worktree remove <path>` after merge, or leave for the user
