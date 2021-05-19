#!/bin/sh
# Restore a tar archive of my home directory
# Usage: ./restore.sh archive.tar.gz extract_dir

tar --listed-incremental=/dev/null \
    --exclude-caches \
    -xvzf \
    "$1" \
    "2"
