# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Export paths
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/Users/matsuyoshi30/.nodebrew/current/bin

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

# Common aliases
alias l='ls -1FG'
alias ll='ls -lG'
alias la='ls -laG'

alias cp="cp -i"
alias mv="mv -i"

alias grep='grep --color=auto'
