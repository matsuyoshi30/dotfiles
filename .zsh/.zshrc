########################################
# 環境変数
export LANG=ja_JP.UTF-8

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

PROMPT="%{${fg[green]}%}[%n@%m] %D{%Y-%m-%d} %*%{${reset_color}%} %~ '${vcs_info_msg_0_}'
%# "

########################################
# vcs_info
autoload -Uz vcs_info
setopt prompt_subst

zstyle ':vcs_info:*' formats '%F{green}(%s)-[%b] %m'
zstyle ':vcs_info:*' actionformats '%F{red}(%s)-[%b|%a] %m'
_vcs_precmd () { vcs_info }
add-zsh-hook precmd _vcs_precmd
zstyle ':vcs_info:git+set-message:*' hooks git-config-user

function +vi-git-config-user(){
  hook_com[misc]+=`git config user.email`
}

function _update_vcs_info_msg() {
    LANG=en_US.UTF-8 vcs_info
    PROMPT="%{${fg[green]}%}[%n@%m] %D{%Y-%m-%d} %*%{${reset_color}%} %~ ${vcs_info_msg_0_}%f
%# "
}

add-zsh-hook precmd _update_vcs_info_msg

########################################
# auto completion
autoload -Uz compinit
compinit

zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
                   /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

fpath=(/usr/local/share/zsh-completions $fpath)
fpath=(/usr/local/share/zsh/site-functions $fpath)


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

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias gst='git status'
alias gd='git diff'
alias ga='git add -p'
