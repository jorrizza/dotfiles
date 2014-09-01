# X resource database
xrdb ~/.Xresources

# X settings
xset s off
xset -dpms

# Audio server.
pulseaudio --start &

# Compositing manager.
#xcompmgr -c -C &

# Clit scrolling and stuff.
for dev in `xinput list |grep pointer |grep TrackPoint | awk -F'id=' '{ print $2 }' | awk '{ print $1 }'`; do
  xinput set-prop $dev "Evdev Wheel Emulation" 1 &
  xinput set-prop $dev "Evdev Wheel Emulation Button" 2 &
  xinput set-prop $dev "Evdev Wheel Emulation Axes" 6 7 4 5 &
done

# Disable the damn touch screen.
xinput set-prop "SYNAPTICS Synaptics Large Touch Screen" "Device Enabled" 0 &

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
eval $(gpg-agent --daemon --enable-ssh-support --disable-scdaemon)

# Fat finger tab caps lock confusion eliminator. Especially when coding drunk.
setxkbmap -option ctrl:nocaps

# Awesomeness.
exec awesome
