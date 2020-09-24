#!/bin/sh
# daily.sh: Backup script run daily

# Configuration variables
name="$(hostname)"
local="/storage/backup/$name"
server="cloud.lagrangian.space"
remote="thalia@$server:~/backup/$name"
ignore=".cache .mozilla .local/share .config"
rsync_exclude=""

# Make sure the destinations are reachable and binaries are installed
[ -w "$local" ] || \
    { echo "daily.sh: Local backup directory unavailable" >&2; exit; }
( ping -c 1 "$server" >/dev/null 2>&1 ) || \
    { echo "daily.sh: Unable to reach backup server" >&2; exit; }
( command -v tar 2>/dev/null ) || \
    { echo "daily.sh: tar not present in path" >&2; exit; }
( command -v rsync 2>/dev/null ) || \
    { echo "daily.sh: rsync not present in path" >&2; exit; }

# Make sure tar knows to ignore certain directories
# Build $rsync_str
for d in $ignore ; do
    [ -d "$HOME"/"$d" ] && \
        echo "Signature: 8a477f597d28d172789f06886806bc55" > \
             "$HOME"/"$d"/CACHEDIR.TAG
    rsync_exclude="$rsync_exclude --exclude '$d'"
done

# Live backups (rsync)
rsync -axv "$rsync_exclude" "$HOME"/ "$local"/"$name"/live
rsync -axv "$local"/"$name"/live/ "$remote"/live
# $remote/$name/live

# Incremental backups (tar)
# $local/$name/
# $remote/$name/
