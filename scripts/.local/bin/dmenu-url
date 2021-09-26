#!/bin/sh
# Grab a url from clipboard and display it with zathura or mpv
# Depends on: xclip, curl, zathura, mpv, youtube-dl, libnotify, and dunst

set -eu

url="$(xclip -o -selection clipboard)"

if [ "$url" = "${url#http}" ] && [ "$url" = "${url#gopher}" ]; then
	notify-send "📕 dmenu-url" "Clipboard doesn't contain a known URL\n$url"
	exit 1
elif [ "${url#"${url%???}"}" = "pdf" ]; then
	echo "$url" | dmenu -p "Display Document?" >/dev/null && {
		notify-send "📕 dmenu-url" "Loading PDF...\n$url"
		curl -s "$url" | zathura - || {
			notify-send "📕 dmenu-url" "Failed to display document."
			exit 1
		}
	}
else
	# Assume it's a video
	echo "$url" | dmenu -p "Play Video?" >/dev/null && {
		notify-send "📕 dmenu-url" "Streaming Video...\n$url"
		# Exit status 4 means mpv was closed prematurely
		notify-send "📕 dmenu-url" "Streaming Video...\n$url"
		mpv "$url" || [ $? != "4" ] && {
			notify-send "📕 dmenu-url" "Failed to play video."
			exit 1
		}
	}
fi
