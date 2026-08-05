#!/usr/bin/env python3
import json
import os
import sys

def find_git_dir(path):
    """Resolve the git dir, following .git files in worktrees."""
    dotgit = os.path.join(path, ".git")
    if os.path.isdir(dotgit):
        return dotgit
    if os.path.isfile(dotgit):
        with open(dotgit) as f:
            line = f.read().strip()
            if line.startswith("gitdir:"):
                return line.split(":", 1)[1].strip()
    return None

def get_branch(path):
    try:
        git_dir = find_git_dir(path)
        if not git_dir:
            return ""
        with open(os.path.join(git_dir, "HEAD")) as f:
            ref = f.read().strip()
            if ref.startswith("ref:"):
                return ref.split("/")[-1]
            return ref[:7]
    except Exception:
        return ""

def relay_context_usage(data):
    """Hand the context-usage numbers to hooks, which never receive them.

    The statusline is the only place Claude Code reports actual usage, and the
    transcript is no substitute: it records the model as `claude-opus-5` whether
    the window is 200k or 1M, so the percentage cannot be recomputed from it.
    Consumed by .claude/hooks/context-handoff-guard.sh.
    """
    session_id = data.get("session_id")
    window = data.get("context_window") or {}
    pct = window.get("used_percentage")
    if not session_id or pct is None:
        return
    state_dir = os.path.join(os.environ.get("TMPDIR", "/tmp"), "claude-ctx")
    os.makedirs(state_dir, exist_ok=True)
    path = os.path.join(state_dir, f"{session_id}.json")
    # Renders can overlap, so swap the file in atomically rather than truncating it.
    tmp = f"{path}.{os.getpid()}"
    with open(tmp, "w") as f:
        json.dump({"pct": pct, "size": window.get("context_window_size")}, f)
    os.replace(tmp, path)

data = json.load(sys.stdin)

try:
    relay_context_usage(data)
except Exception:
    pass

model = data.get("model", {}).get("display_name", "Claude")
current_dir = data.get("workspace", {}).get("current_dir", "")
dirname = os.path.basename(current_dir) if current_dir else "?"

branch_name = get_branch(current_dir) if current_dir else ""
branch = f" | 🌿 {branch_name}" if branch_name else ""

ctx = data.get("context_window", {}).get("used_percentage")
five = data.get("rate_limits", {}).get("five_hour", {}).get("used_percentage")
seven = data.get("rate_limits", {}).get("seven_day", {}).get("used_percentage")

parts = [f"[{model}] 📁 {dirname}{branch}"]
if ctx is not None:
    parts.append(f"ctx {round(ctx)}%")
if five is not None:
    parts.append(f"5h {round(five)}%")
if seven is not None:
    parts.append(f"7d {round(seven)}%")

print(" | ".join(parts), end="")
