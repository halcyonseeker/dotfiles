#!/bin/sh
# daily.sh: Backup script run daily
# Depends on rsync and optionally libnotify and a notification daemon

# Exit on any error
set -e

message_function()
{
    echo "$1"
    notify-send "🏠 -> 💾 daily-backup.sh" "$1" >/dev/null
}

# Configuration variables
remote="thalia@192.168.5.19:~/backup"
local="/storage/backups/$(hostname)"
ignore=".cache .mozilla .local/share .config .emacs.d/backups .emacs.d/auto-save-list .emacs.d/elpa .tor-browser"

# Tell tar to ignore some directories
# TODO: Why can't I just pass an exclude string to rsync?
for d in $ignore ; do
    [ -d "$HOME"/"$d" ] && \
        echo "Signature: 8a477f597d28d172789f06886806bc55" > \
             "$HOME"/"$d"/CACHEDIR.TAG
done
 
# Live backups (rsync)
rsync -axv \
      --delete-after \
      --exclude '.cache' \
      --exclude '.mozilla' \
      --exclude '.local/share' \
      --exclude '.config' \
      --exclude '.emacs.d/backups' \
      --exclude '.emacs.d/auto-save-list' \
      --exclude '.emacs.d/elpa' \
      --exclude 'temporary' \
      --exclude '.tor-browser' \
      "$HOME"/ "$local"/live

case $? in
    0) message_function "Live backup succeeded." ;;
    *) message_function "Live backup failed." ;;
esac

# Create differential tarball from the last weekly snapshot
# pass -level=0 to take a clean snapshot
tar --listed-incremental="$local"/snapshot.file \
    --exclude-caches \
    -cvzf \
    "$local"/daily/backup-"$(date +'%F-%s')".1.tar.gz \
    "$HOME"

case $? in
    0) message_function "Tar snapshot succeeded." ;;
    *) message_function "Tar snapshot failed." ;;
esac

# Copy that all to remote
rsync -axv "$local" "$remote"/

case $? in
    0) message_function "Remote backup succeeded." ;;
    *) message_function "Remote backup failed." ;;
esac
