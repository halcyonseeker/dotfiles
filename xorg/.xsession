# Stuff I want to run whenever any WM or DE is started.

# Don't beep at me
xset b off

# Disable ThinkPad touchpad
xinput disable "$(xinput list | awk '/TouchPad/{sub(/id=/, "", $6); print $6}')"

# Make trackpoint move faster
xinput set-prop "$(xinput list | awk '/TrackPoint/{sub(/id=/, "", $6); print $6}')" 'libinput Accel Speed' 1

# Scroll with ThinkPad middle button in OpenBSD
[ "$(uname)" = "OpenBSD" ] && {
	xinput --set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation" 1
	xinput --set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Button" 2
	xinput --set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Axes" 6 7 4 5
}

# Load Xresources
xrdb -merge ~/.config/xorg/Xresources

# Swap caps lock and escape and set a compose key
setxkbmap -option caps:swapescape
setxkbmap -option compose:ralt

# Remove this if using a display manager
exec i3
