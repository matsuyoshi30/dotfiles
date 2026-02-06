---
name: tmux-sender
description: Send commands to another tmux pane. Use for requests like "run in pane" or "send via tmux".
allowed-tools: Bash(tmux:*)
---

# tmux Command Sender Skill

## Usage

To send and execute a command in a tmux pane:

tmux send-keys -t <pane-number> '<command>' Enter

## Steps

1. Run `tmux list-panes` to check available panes
2. Run `tmux send-keys -t <pane-number> '<command>' Enter` to send and execute
