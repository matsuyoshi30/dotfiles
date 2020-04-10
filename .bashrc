# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Export paths
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin:$HOME/.nodebrew/current/bin

# Prompt setting
source /usr/local/etc/bash_completion.d/git-prompt.sh

GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWUNTRACKEDFILES=true
GIT_PS1_SHOWSTASHSTATE=true
GIT_PS1_SHOWUPSTREAM=auto

function parse_git_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ [\1]/'
}
function promps {
    local  BLUE="\[\e[1;34m\]"
    local  RED="\[\e[1;31m\]"
    local  GREEN="\[\e[1;32m\]"
    local  WHITE="\[\e[00m\]"
    local  GRAY="\[\e[1;37m\]"

    case $TERM in
        xterm*) TITLEBAR='\[\e]0;\W\007\]';;
        *)      TITLEBAR="";;
    esac
    local BASE="\u"
    local TIME="\t"
    PS1="\n${BLUE}${TIME} \w ${GREEN}\u ${RED}\$(parse_git_branch)\n${BLUE}$ \[\033[0m\]"
}
promps
export PROMPT_COMMAND='echo -ne "\033];${PWD##*/}\007"'

# less
export PAGER=less
export LESS_TERMCAP_mb=$'\E[01;31m'      # Begins blinking.
export LESS_TERMCAP_md=$'\E[01;31m'      # Begins bold.
export LESS_TERMCAP_me=$'\E[0m'          # Ends mode.
export LESS_TERMCAP_se=$'\E[0m'          # Ends standout-mode.
export LESS_TERMCAP_so=$'\E[00;47;30m'   # Begins standout-mode.
export LESS_TERMCAP_ue=$'\E[0m'          # Ends underline.
export LESS_TERMCAP_us=$'\E[01;32m'      # Begins underline.

# Common aliases
alias l='ls -1FG'
alias ls='ls -1FG'
alias ll='ls -lFG'
alias la='ls -laFG'
alias lt='ls -ltFG'
alias lta='ls -ltaFG'

alias cp="cp -i"
alias mv="mv -i"

alias grep='grep --color=auto'

alias gst='git status'
