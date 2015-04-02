#
# .profile - This file is envoked by bourne-shell variants (including
#            bash) on login.

# Get the aliases and functions from your .bashrc
if [ -f ~/.bashrc ]; then
        . ~/.bashrc
fi

# Set the default editor you prefer
#EDITOR=emacs
#EDITOR=vim
EDITOR=pico
export EDITOR
