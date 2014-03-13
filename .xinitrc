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

# Disable the damn touch screen and touch pad.
xinput set-prop "N-Trig Touchscreen" "Device Enabled" 0 &
xinput set-prop "N-Trig Pen" "Device Enabled" 0 &
xinput set-prop "N-Trig MultiTouch" "Device Enabled" 0 &
xinput set-prop "SynPS/2 Synaptics TouchPad" "Device Enabled" 0 &

# Clean up slow downers.
rm -f ~/.recently-used.xbel
rm -f ~/.recently-used
rm -f ~/.local/share/recently-used.xbel
rm -f ~/.local/share/recently-used

# Fix the display
[[ -x .screenlayout/default.sh ]] && .screenlayout/default.sh

# Fix stupid X clients (from suckless-tools)
wmname LG3D &

# Awesomeness.
exec awesome
