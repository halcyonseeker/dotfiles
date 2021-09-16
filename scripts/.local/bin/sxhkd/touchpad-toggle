#!/bin/sh
# Toggle my touchpad on and off
# Depends on xinput

set -eu

id="$(xinput --list | grep Synaptics | cut -f2 | tr -d "s/id=//")"
case "$(xinput --list-props "$id" | grep 'Device Enabled' | cut -f3)" in
	0) xinput --enable "$id" ;;
	*) xinput --disable "$id" ;;
esac
