#!/bin/sh
# Backup files in /storage directory to external drive mounted at /mnt
# Depends on rsync and optionally libnotify and a notification daemon like dunst

# Exit on any error or when a child process is interupted
set -eu
trap 'exit 0' INT

df | grep storage >/dev/null || {
	message_function "Error: Storage parition not mounted at /storage"
	exit 1
}

df | grep mnt >/dev/null || {
	message_function "Error: Backup drive not mounted at /mnt"
	exit 1
}

[ -w /mnt ] || {
	message_function "Error: /mnt is not writable by the curent user"
	exit 1
}

message_function () {
	echo "$1"
	command -v notify-send >/dev/null && \
		notify-send "🖥 -> 💾 storage-backup.sh" "1"
}

echo "
list+found
.Trash-1000
" | rsync -axv --delete-after --exclude-from - /storage/ /mnt \
	&& message_function "Storage backup succeeded"

