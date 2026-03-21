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

data = json.load(sys.stdin)

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
