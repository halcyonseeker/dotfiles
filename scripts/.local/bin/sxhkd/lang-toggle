#!/bin/sh
# Toggle between US and Russian keyboard input. I bind this to Mod4+F1
# Depends on setxkbmap
# TODO figure out Russian keyboard in the console

set -eu

if [ "$(setxkbmap -query | grep layout | awk '{print $2}')" = "us" ]; then
	setxkbmap -model ru -layout ru
else
	setxkbmap -model us -layout us
fi

grep setxkbmap "${XINITRC:-$HOME/.config/xorg/xinitrc}" | sh
