# No screen blanking
xset -dpms
xset s off

# X resource database
xrdb ~/.Xresources

# Firefox on X11
unset MOZ_ENABLE_WAYLAND
firefox -P X11 &

# Terminal
x-terminal-emulator &

# Teams!
teams

# Window manager
exec openbox
