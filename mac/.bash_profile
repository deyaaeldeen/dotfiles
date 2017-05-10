#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
if [ -e /Users/leapyear-deyaa/.nix-profile/etc/profile.d/nix.sh ]; then .  /Users/leapyear-deyaa/.nix-profile/etc/profile.d/nix.sh; fi
