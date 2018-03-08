# X resource database
xrdb ~/.Xresources

# X settings
xset s off
xset -dpms

# Clean up slow downers.
rm -f ~/.recently-used.xbel
rm -f ~/.recently-used
rm -f ~/.local/share/recently-used.xbel
rm -f ~/.local/share/recently-used

# Fix the display
test -x ~/.screenlayout/default.sh && ~/.screenlayout/default.sh

# Fix stupid X clients (from suckless-tools)
wmname LG3D &

# GPG needs to know this stuff
echo "UPDATESTARTUPTTY" | gpg-connect-agent
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"

# Let systemd and dbus know about our environment
dbus-update-activation-environment --verbose --systemd --all

# Fat finger tab caps lock confusion eliminator. Especially when coding drunk.
setxkbmap -option ctrl:nocaps

# Notification daemon
dunst &

# Window manager
exec i3
