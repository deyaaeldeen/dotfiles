#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export EDITOR="emacs"

umask 077

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=32768
HISTFILESIZE=32768
#export HISTIGNORE="cd [a-zA-Z0-9_.*]*:mv [a-zA-Z0-9_.*]*:cp [a-zA-Z0-9_.*]*:.*"

#PS1='[\u@\h \W]\$ '
export PS1="\u@\h \w\$ "
export PROMPT_COMMAND="date"

export LANG=en_US.UTF-8
export PATH=$PATH:~/.cabal/bin

alias l='ls -lhFa'
alias ..='cd ..'
alias ...='cd ../../../'
alias ....='cd ../../../../'
alias .....='cd ../../../../'
alias .4='cd ../../../../'
alias .5='cd ../../../../..'
alias k="killall -9"
alias h='cd ~'
alias hh='history'
alias rm='rm -rf'

alias gs="git status"
alias gc="git commit -am"
alias gh="git push"
alias gl="git pull"
alias glg='git log'
alias gcn='git clean -xfd'
alias gdr='git push origin :'

alias rakt='racket -il xrepl'

extract () {
     if [ -f $1 ] ; then
         case $1 in
             *.tar.bz2)   tar xjf $1        ;;
             *.tar.gz)    tar xzf $1     ;;
             *.bz2)       bunzip2 $1       ;;
             *.rar)       rar x $1     ;;
             *.gz)        gunzip $1     ;;
             *.tar)       tar xf $1        ;;
             *.tbz2)      tar xjf $1      ;;
             *.tgz)       tar xzf $1       ;;
             *.zip)       unzip $1     ;;
             *.Z)         uncompress $1  ;;
             *.7z)        7z x $1    ;;
             *.tar.xz)        tar -xvJf $1    ;;	     
             *)           echo "'$1' cannot be extracted via extract()" ;;
         esac
     else
         echo "'$1' is not a valid file"
     fi
}

sr ()
{
    find $1 -type f -exec sed -i "s/$2/$3/g" {} \;
}

