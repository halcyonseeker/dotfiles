#!/bin/sh
# daily.sh: Backup script run daily
# Depends on rsync and optionally libnotify and a notification daemon

# Exit on any error or when a child process is interupted
set -eu
trap 'exit 1' INT

message_function() {
	echo "$1"
	command -v notify-send >/dev/null && \
		notify-send "🏠 -> 💾 daily-backup.sh" "$1"
}

local="/storage/backups/$(hostname)"
[ -w "$local" ] || {
	message_function "Error: backup dir is not writable"
	exit 1
}
mkdir -p "$local"/live 2>/dev/null

echo "
.cache
.local/cargo
.local/rustup
.local/go
.local/lib
.slime
.tor-browser
repos
" | rsync -axv --delete-after --exclude-from - \
	"$HOME"/ "$local"/live && message_function "Live backup succeeded."

# Copy the snapshots and live copy to my remote server
rsync -axv "$local"/ thalia@ulthar.xyz:~/backup/ \
	&& message_function "Remote backup succeeded."

