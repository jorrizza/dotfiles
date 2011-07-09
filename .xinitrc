# Compositing manager.
#xcompmgr -c -C &

# Background.
awsetbg -l &

# Clit scrolling and stuff.
for dev in `xinput list |grep pointer |grep TrackPoint | awk -F'id=' '{ print $2 }' | awk '{ print $1 }'`; do
  xinput set-prop $dev "Evdev Wheel Emulation" 1 &
  xinput set-prop $dev "Evdev Wheel Emulation Button" 2 &
  xinput set-prop $dev "Evdev Wheel Emulation Axes" 6 7 4 5 &
done

# Disable the damn touch screen.
xinput set-prop "N-trig DuoSense" "Device Enabled" 0 &

# Clean up slow downers.
rm -f ~/.recently-used.xbel
rm -f ~/.recently-used
rm -f ~/.local/share/recently-used.xbel
rm -f ~/.local/share/recently-used

# Fix the display
arandr &

# Awesomeness.
exec awesome
