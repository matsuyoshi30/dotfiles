########################################
# 環境変数
export LANG=ja_JP.UTF-8
export EDITOR=hx

# 色を使用出来るようにする
autoload -Uz colors
colors

# use emacs keybindings
bindkey -e

# history setting
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

########################################
# prompt
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
autoload -Uz compinit
compinit

zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
       /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

if [[ $(uname -m) == "x86_64" ]]; then
  fpath=(/usr/local/share/zsh-completions $fpath)
  fpath=(/usr/local/share/zsh/site-functions $fpath)
else
  fpath=(/opt/homebrew/share/zsh-completions $fpath)
  fpath=(/opt/homebrew/share/zsh/site-functions $fpath)
fi


########################################
# Options

setopt print_eight_bit  # 日本語ファイル名を表示可能にする
setopt interactive_comments  # '#' 以降をコメントとして扱う
setopt share_history  # 同時に起動したzshの間でヒストリを共有する
setopt hist_ignore_all_dups  # 同じコマンドをヒストリに残さない
setopt hist_ignore_space  # スペースから始まるコマンド行はヒストリに残さない
setopt hist_reduce_blanks  # ヒストリに保存するときに余分なスペースを削除する

########################################
# less
alias less='less -M'
export PAGER=less
export LESS_TERMCAP_mb=$'\E[01;31m'      # Begins blinking.
export LESS_TERMCAP_md=$'\E[01;31m'      # Begins bold.
export LESS_TERMCAP_me=$'\E[0m'          # Ends mode.
export LESS_TERMCAP_se=$'\E[0m'          # Ends standout-mode.
export LESS_TERMCAP_so=$'\E[00;47;30m'   # Begins standout-mode.
export LESS_TERMCAP_ue=$'\E[0m'          # Ends underline.
export LESS_TERMCAP_us=$'\E[01;32m'      # Begins underline.

########################################
# ghq
peco-src () {
    local repo=$(ghq list | peco --query "$LBUFFER")
    if [ -n "$repo" ]; then
        repo=$(ghq list --full-path --exact $repo)
        BUFFER="cd ${repo}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N peco-src
bindkey '^]' peco-src


########################################
# git-wt
eval "$(git wt --init zsh)"

peco-wt () {
    local wt=$(git wt | tail -n +2 | peco | awk '{print $(NF-1)}')
    if [ -n "$wt" ]; then
        BUFFER="git wt ${wt}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N peco-wt
bindkey '^\' peco-wt

########################################
# Alias
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
alias gco='git branch|peco|xargs git checkout'
alias gdl='git branch|peco|xargs git branch -D'
alias gdmb="git branch --merged | grep -vE '(master|main|develop)' | xargs -n1 git branch -D"

alias ghb='gh browse'

alias E='open -a Emacs.app'

alias k='kubectl'

if [[ -x "$HOME/.claude/local/claude" ]]; then
    alias claude="$HOME/.claude/local/claude"
fi

# version managers
if command -v anyenv &> /dev/null; then
  eval "$(anyenv init -)"
fi

if command -v rbenv &> /dev/null; then
  eval "$(rbenv init -)"
fi

if command -v nodenv &> /dev/null; then
  eval "$(nodenv init -)"
fi

# direnv
if command -v direnv &> /dev/null; then
  eval "$(direnv hook zsh)"
fi

# opam
[[ ! -r "$HOME/.opam/opam-init/init.zsh" ]] || source "$HOME/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null

# ghcup
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

# Added by Antigravity
export PATH="$HOME/.antigravity/antigravity/bin:$PATH"

# Amp CLI
export PATH="$HOME/.amp/bin:$PATH"
