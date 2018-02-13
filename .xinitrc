# X resource database
xrdb ~/.Xresources

# X settings
xset s off
xset -dpms

# Audio server.
pulseaudio --start &

# Touchpad daemon disables touchpad when typing
syndaemon -k -i 2 &

# Clean up slow downers.
rm -f ~/.recently-used.xbel
rm -f ~/.recently-used
rm -f ~/.local/share/recently-used.xbel
rm -f ~/.local/share/recently-used

# Fix the display
test -x ~/.screenlayout/default.sh && ~/.screenlayout/default.sh

# Fix stupid X clients (from suckless-tools)
wmname LG3D &

# GPG
# Before systemd:
# eval $(gpg-agent --daemon --enable-ssh-support --disable-scdaemon)
# After systemd:
# systemctl --user enable gpg-agent.socket
# systemctl --user enable gpg-agent-ssh.socket
echo "UPDATESTARTUPTTY" | gpg-connect-agent
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"

# Fat finger tab caps lock confusion eliminator. Especially when coding drunk.
setxkbmap -option ctrl:nocaps

# Window manager
exec i3
