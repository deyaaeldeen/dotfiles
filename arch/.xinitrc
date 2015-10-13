#!/bin/sh

#exec awesome
xsetroot -cursor_name left_ptr
feh --bg-scale ~/bat.jpg
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
xbindkeys
exec xmonad
