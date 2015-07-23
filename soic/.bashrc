#
# .bashrc
#

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# set the umask, which controls the permissions on new files you create.
#   022 denies write access to other people, but not read access.
#   077 denies read and write access to other people.
umask 077

export schmlUnderConstruction=1

export PATH="$HOME/bin/bin:$HOME/.cabal/bin:/u/parfunc/opt/bin:$PATH"
export LANG=en_US.UTF-8
LD_LIBRARY_PATH=/u/dalmahal/lib/lib
export LD_LIBRARY_PATH

module load emacs/24.4 gcc/4.7.2 ghc/7.8.3 python/2.7.5 racket/6.1.1 git/1.8.5.6

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=32768
HISTFILESIZE=32768
#export HISTIGNORE="cd [a-zA-Z0-9_.*]*:mv [a-zA-Z0-9_.*]*:cp [a-zA-Z0-9_.*]*:.*"

alias ls='ls --color=auto'
export PS1="\u@\h \w\$ "
export PROMPT_COMMAND="date"

alias l='ls -lhFa'
alias c='reset'
alias r='sudo reboot'
alias ..='cd ..'
alias ...='cd ../../../'
alias ....='cd ../../../../'
alias .....='cd ../../../../'
alias .4='cd ../../../../'
alias .5='cd ../../../../..'
alias k="killall -9"
alias h='cd ~'
alias rm='rm -rf'

alias gs="git status"
alias gc="git commit -am"
alias gh="git push"
alias gl="git pull"

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

shot ()
{
import -frame -strip -quality 75 "$HOME/$(date +%s).png"
}
