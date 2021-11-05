#!/bin/sh
# Show an emoji picker in dmenu. At some point I might make this fancier.
# Depends on dmenu, xclip, libnotify, a notification daemon and an emoji font

# TODO: this script should be able to automatically build the emoji file
# curl -s https://unicode.org/emoji/charts/full-emoji-list.html \
	# | grep -e "^<td class='code'><a href='.*' name='.*'>.*</a></td>$" \
	       # -e "^<td class='chars'>.</td>$" \
	       # -e "^<td class='name'>.*</td>$" \
	# | sed -e "s/^<td class='code'><a href='.*' name='.*'>//" \
	      # -e "s/^<td class='chars'>//" \
	      # -e "s/^<td class='name'>//" \
	      # -e "s/<\/td>//; s/<\/a>//" \
	      # -e "s/\&amp;/\&/"
# Now place the emoji, code, and description on the same line

set -eu

emojis="${XDG_DATA_HOME:-$HOME/.local/share}"/dotfiles/emojis.list
line="$(grep -v "#" "$emojis" | rofi -dmenu)" || exit
echo "$line" | cut -d " " -f 1 | tr -d "\n" | xclip -selection clipboard && {
	notify-send -t 2000 "$(xclip -o -selection clipboard) copied to clipboard"
}
