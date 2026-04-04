typeset -U path PATH

export LANG=ja_JP.UTF-8
export EDITOR=hx

export PAGER=less
export LESS_TERMCAP_mb=$'\E[01;31m'      # Begins blinking.
export LESS_TERMCAP_md=$'\E[01;31m'      # Begins bold.
export LESS_TERMCAP_me=$'\E[0m'          # Ends mode.
export LESS_TERMCAP_se=$'\E[0m'          # Ends standout-mode.
export LESS_TERMCAP_so=$'\E[00;47;30m'   # Begins standout-mode.
export LESS_TERMCAP_ue=$'\E[0m'          # Ends underline.
export LESS_TERMCAP_us=$'\E[01;32m'      # Begins underline.

path=(
  "$HOME/.browser-use-env/bin"
  "$HOME/.cargo/bin"
  "$HOME/Library/pnpm"
  "$HOME/.amp/bin"
  "$HOME/.antigravity/antigravity/bin"
  "$HOME/.bun/bin"
  "$HOME/bin"
  "$HOME/.local/bin"
  "$HOME/.nodebrew/current/bin"
  "$HOME/go/bin"
  "/Library/Java/JavaVirtualMachines/temurin-21.jdk/Contents/Home/bin"
  "$HOME/Library/Android/sdk/platform-tools"
  "$HOME/Library/Application Support/Coursier/bin"
  "$HOME/.anyenv/bin"
  "$HOME/.nodenv/bin"
  "$path[@]"
)

export PNPM_HOME="$HOME/Library/pnpm"
export GOPATH="$HOME/go"
export JAVA_PATH="/Library/Java/JavaVirtualMachines/temurin-21.jdk/Contents/Home"
export ANDROID_HOME="$HOME/Library/Android/sdk"
export BUN_INSTALL="$HOME/.bun"
export ORGSYNCROOT="$HOME/Dropbox"
export CLAUDE_CONFIG_DIR="$HOME/.claude"
export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"
export AWS_DEFAULT_PROFILE=saml
