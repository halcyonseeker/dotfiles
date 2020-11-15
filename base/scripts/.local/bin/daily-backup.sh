#!/bin/sh
# daily.sh: Backup script run daily

# Exit on any error
set -e

# Configuration variables
remote="thalia@cloud.lagrangian.space:~/backup/$(hostname)"
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

# Create differential tarball from the last weekly snapshot
# pass -level=0 to take a clean snapshot
tar --listed-incremental="$local"/snapshot.file \
    --exclude-caches \
    -cvzf \
    "$local"/daily/backup-$(date +'%F-%s').1.tar.gz \
    $HOME

# Copy that all to remote
rsync -axv "$local" "$remote"/
