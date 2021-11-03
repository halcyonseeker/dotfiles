#!/bin/sh
# A wrapper around system-specific volume controls; called by Fvwm's
# keyboard daemon
#
# Usage: audio-abstract [up|down|mute]

notify() {
	command -v notify-send >/dev/null || return
	pgrep x dbus-launch >/dev/null || return

	notify-send "🔊 Volume" "$1"
}

case "$(uname)" in
	FreeBSD)
		b="$(mixer | awk '/vol/ {sub("^[0-9]*:", "", $7); print $7}')"
		case "$1" in
			up)   mixer vol +10 ;;
			down) mixer vol -10 ;;
			mute) true ;; # Handled elsewhere; see snd(4)
		esac
		a="$(mixer | awk '/vol/ {sub("^[0-9]*:", "", $7); print $7}')"
		notify "$b -> $a"
		;;
	Linux)
		case "$1" in
			up)   amixer set Master 10%+   ;;
			down) amixer set Master 10%-   ;;
			mute) amixer set Master toggle ;;
		esac
		;;
esac
