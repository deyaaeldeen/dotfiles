#exec awesome
xsetroot -cursor_name left_ptr
exec xmonad
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
xloadimage -onroot -fullscreen -border black bat.jpg
