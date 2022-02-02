#!/bin/sh

set -e

install_file() {
        file="$1"
        trgt="$2"
        link="$3"
        [ -L "$link" ] && [ "$(realpath "$link")" = "$trgt" ] && return
        echo "  Creating link ~/$file..."
        mkdir -p "$(dirname "$link")"
        ln -s "$trgt" "$link"
}

do_package() {
        func="$1"
        pack="$(basename "$2")"
        echo "==========> $pack..."
        find "$pack" -type f | while read -r f; do
                file="${f##$pack/}"
                link="$HOME/$file"
                trgt="$(realpath "$f")"
                "$func" "$file" "$trgt" "$link"
        done
}

main() {
        if [ $# -gt 0 ]; then
                for p in "$@"; do
                        do_package "$action" "$p"
                        shift
                done
        else
                find . -mindepth 1 -maxdepth 1 -type d \
                        | grep -v '^\./\.' \
                        | while read -r p; do do_package "$action" "$p"; done
        fi
}

if [ "$(basename "$0")" = "mk" ]; then
        action="install_file" main "$@"
fi

