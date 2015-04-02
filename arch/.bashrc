#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

xrdb -merge .Xresources

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=2000
HISTFILESIZE=4000

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export LANG=en_US.UTF-8
export PATH=$PATH:/home/deyaa/.cabal/bin
export PATH=$PATH:/usr/local/texlive/2014/bin/x86_64-linux
export INFOPATH=$INFOPATH:/usr/local/texlive/2014/texmf-dist/doc/info
export MANPATH=$MANPATH:/usr/local/texlive/2014/texmf-dist/doc/man

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

alias wrk='cd /mnt/disk/Work/Indiana/gradual/'
alias sd='cd /mnt/disk/Study/Indiana/B522'
alias t='cd /mnt/disk/Teaching/p423'
alias is="/mnt/disk/Study/Indiana/B522/./isabelle.sh"

alias gs="git status"
alias gc="git commit -am"
alias gh="git push"
alias gl="git pull"

alias p-s='sudo pacman -S'      #install
alias p-syu='sudo pacman -Syu'  #sync refresh sys update
alias p-rs='sudo pacman -Rs'    #remove plus unused dependencies
alias p-scc='sudo pacman -Scc'  #clean cache - all pkgs
alias p-sc='sudo pacman -Sc'    #clean cache - old pkgs only
alias p-ss='sudo pacman -Ss'    #query database
alias p-qs='sudo pacman -Qs'    #query installed only
alias p-si='sudo pacman -Si'    #pkg info
alias p-qi='sudo pacman -Qi'    #pkg more info
alias p-qe='sudo pacman -Qe'    #list explicitely installed pkgs
alias p-ql='sudo pacman -Ql'    #find pkg file list
alias p-qo='sudo pacman -Qo'    #/path/to/file = find owner
alias p-sf='sudo pacman -Sf'    #reinstall - for dep problem

alias a-syu='aurget -Syu'
alias a-s='aurget -S'

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
