# ~/.xsession -- Stuff I want to run whenever any WM or DE is started

# Don't beep at me
xset b off

# Disable ThinkPad touchpad
xinput --disable "$(xinput --list | grep Synaptics | awk '{print $6}' | tr -d 'id=')"

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
