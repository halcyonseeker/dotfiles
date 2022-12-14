#!/bin/sh

notify() {
	command -v notify-send >/dev/null || return
	pgrep -x dbus-launch >/dev/null || return
	notify-send "🔓 Password Store" "$1"
}

dir="${PASSWORD_STORE_DIR:-$HOME/.password-store}"

item="$(
	find "$dir" -type f -name "*.gpg" \
		| awk -v dir="$dir"/ '{
			sub(dir, "", $1)
			sub(/\.gpg$/, "", $1)
			print $1}' \
		| rofi -dmenu -p "Copy Password"
)"

[ -z "$item" ] && exit 1

pass "$item" | head -1 | tr -d '\n' | xclip -selection clipboard
notify "Copied <b>$item</b> to clipboard.\nClearing in 60 seconds..."
sleep 60
echo -n | xclip -selection clipboard
