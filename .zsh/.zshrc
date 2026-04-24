autoload -Uz colors
colors

bindkey -e

HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

autoload -Uz add-zsh-hook

########################################
# vcs_info
autoload -Uz vcs_info
setopt prompt_subst

zstyle ':vcs_info:*' formats '%F{green}(%s)-[%b] %m'
zstyle ':vcs_info:*' actionformats '%F{red}(%s)-[%b|%a] %m'
zstyle ':vcs_info:git+set-message:*' hooks git-config-user

function git-config-user(){
  hook_com[misc]+=`git config user.email`
}

function shpwd() {
  echo ${${:-/${(j:/:)${(M)${(s:/:)${(D)PWD:h}}#(|.)[^.]}}/${PWD:t}}//\/~/\~}
}

function _update_vcs_info_msg() {
  LANG=en_US.UTF-8 vcs_info
  local p_info="%{${fg[green]}%}%D{%Y-%m-%d} %*%{${reset_color}%}"
  local p_dir="$(shpwd) ${vcs_info_msg_0_}%f"$'\n'
  local p_mark="%B%(?,%{${fg[green]}%},%F{red})%(!,#,%%)%f%b"
  PROMPT="$p_info $p_dir$p_mark "
}

add-zsh-hook precmd _update_vcs_info_msg

########################################
# auto completion
# Completion directories depend on where Homebrew is installed, not on shell mode.
if [[ -d /opt/homebrew/share/zsh-completions ]]; then
  fpath=(/opt/homebrew/share/zsh-completions $fpath)
fi

if [[ -d /opt/homebrew/share/zsh/site-functions ]]; then
  fpath=(/opt/homebrew/share/zsh/site-functions $fpath)
fi

if [[ -d /usr/local/share/zsh-completions ]]; then
  fpath=(/usr/local/share/zsh-completions $fpath)
fi

if [[ -d /usr/local/share/zsh/site-functions ]]; then
  fpath=(/usr/local/share/zsh/site-functions $fpath)
fi

autoload -Uz compinit
compinit -u

zstyle ':completion:*:sudo:*' command-path /opt/homebrew/sbin /opt/homebrew/bin \
       /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

########################################
# Options

setopt print_eight_bit  # Allow displaying Japanese filenames
setopt interactive_comments  # Treat '#' as comment in interactive shells
setopt share_history  # Share history across concurrent zsh sessions
setopt hist_ignore_all_dups  # Remove older duplicate entries from history
setopt hist_ignore_space  # Skip commands starting with a space from history
setopt hist_reduce_blanks  # Strip extra whitespace when saving history

########################################
# ghq
fzf-src () {
    local repo=$(ghq list | fzf --query "$LBUFFER")
    if [ -n "$repo" ]; then
        repo=$(ghq list --full-path --exact $repo)
        BUFFER="cd ${repo}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N fzf-src
bindkey '^]' fzf-src

########################################
# git-wt
if command -v git-wt >/dev/null 2>&1; then
  eval "$(git-wt --init zsh)"
fi

gwt() {
  local root=$(git rev-parse --show-toplevel)
  local wt=$(git wt | tail -n +2 | sed "s|${root}/|./|g; s|${root}|.|g" | fzf | awk '{print $(NF-1)}')
  if [ -n "$wt" ]; then
    git wt ${wt}
  fi
}

gwtdl() {
  local root=$(git rev-parse --show-toplevel)
  local wt=$(git wt | tail -n +2 | sed "s|${root}/|./|g; s|${root}|.|g" | fzf | awk '{print $1}')
  if [ -n "$wt" ]; then
    git worktree remove "${root}/${wt#./}"
  fi
}

########################################
# Alias
alias less='less -M'

alias l='ls -1FG'
alias ls='ls -1FG'
alias ll='ls -lFG'
alias la='ls -laFG'
alias lt='ls -ltFG'
alias lta='ls -ltaFG'

alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

alias gst='git status'
alias gd='git diff'
alias ga='git add -p'
alias gpu='git push'
alias glst='git log --stat'
alias glo='git log --oneline'
alias glos='git log --oneline --stat'
alias glon='git log --oneline --name-only'
alias gco='git branch|fzf|xargs git checkout'
alias gdl='git branch|fzf|xargs git branch -D'
alias gdmb="git branch --merged | grep -vE '(master|main|develop)' | xargs -n1 git branch -D"
alias gdmwt='git worktree list | while read -r path commit branch; do b=${branch#[}; b=${b%]}; if echo "$b" | grep -qvE "^(master|main|develop)$" && git branch --merged | grep -qw "$b"; then git worktree remove "$path"; fi; done'

alias ghb='gh browse'

alias lg='lazygit'

alias E='open -a Emacs.app'

alias k='kubectl'

alias cl='cage claude'

if [[ -x "$HOME/.claude/local/claude" ]]; then
    alias claude="$HOME/.claude/local/claude"
fi

if command -v anyenv &> /dev/null; then
  eval "$(anyenv init -)"
fi

if command -v rbenv &> /dev/null; then
  eval "$(rbenv init -)"
fi

if command -v nodenv &> /dev/null; then
  eval "$(nodenv init -)"
fi

if command -v direnv &> /dev/null; then
  eval "$(direnv hook zsh)"
fi

[[ ! -r "$HOME/.opam/opam-init/init.zsh" ]] || source "$HOME/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null

[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"

[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"
