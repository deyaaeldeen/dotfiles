#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export EDITOR="emc"

umask 077

# Schml compiler
export schmlUnderConstruction=1

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=32768
HISTFILESIZE=32768
#export HISTIGNORE="cd [a-zA-Z0-9_.*]*:mv [a-zA-Z0-9_.*]*:cp [a-zA-Z0-9_.*]*:.*"

alias ls='ls --color=auto'
#PS1='[\u@\h \W]\$ '
export PS1="\u@\h \w\$ "
export PROMPT_COMMAND="date"

export LANG=en_US.UTF-8
export PATH=$PATH:/home/deyaa/.cabal/bin:/usr/local/texlive/2014/bin/x86_64-linux
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
alias hh='history'
alias rm='rm -rf'

alias img='sxiv'
alias 'cdrom'='sudo mount -a /dev/sr0' # pass the path to mount the cd in
alias wrk='cd /mnt/disk/Work/Indiana/gradual/'
alias sd='cd /mnt/disk/Study/Indiana'
alias is="/mnt/disk/Study/Indiana/B522/./isabelle.sh"
alias t='xterm -e /bin/fish'
alias XTERM_SHELL=/bin/fish

alias gs="git status"
alias gc="git commit -am"
alias gh="git push"
alias gl="git pull"
alias glg='git log'
alias gcn='git clean -xfd'
alias gdr='git push origin :'

alias p-s='sudo pacman -S'      #install
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
alias syu='sudo pacman -Syu;aurget --deps -Syu' # system upgrade
alias a-s='aurget --noconfirm --noedit --deps -S'

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

shot ()
{
import -frame -strip -quality 75 "$HOME/$(date +%s).png"
}

sr ()
{
    find $1 -type f -exec sed -i "s/$2/$3/g" {} \;
}

prjcton ()
{
    xrandr --output HDMI1 --mode 1280x800;
}

prjctoff ()
{
    xrandr --output HDMI1 --off;
}
