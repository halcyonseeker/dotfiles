#!/bin/sh
# Backup files in /storage directory to external drive mounted at /mnt
# Depends on rsync and optionally libnotify and a notification daemon like dunst

message_function () {
    echo "$1"
    notify-send "🖥 -> 💾 storage-backup.sh" "$1" 2>/dev/null
}

# Check if volumes are mounted
[ "$(df | grep -e mnt -e storage -c)" = 2 ] || \
    {
        message_function "ERROR: Storage partition (/storage) or backup drive (/mnt) not mounted"
        exit 1
    }

# Make sure I can write to /mnt
if (touch /mnt/storage-backup-sh-testfile >/dev/null 2>&1) ; then
    rm -f /mnt/storage-backup-sh-testfile
else
    message_function "ERROR: unable to write to /mnt"
    exit 1
fi

rsync -axv \
      --delete-after \
      --exclude 'lost+found' \
      --exclude '.Trash-1000' \
      /storage/ /mnt

case $? in
    0) message_function "Storage backup succeeded" ;;
    *) message_function "Storage backup failed" ;;
esac
