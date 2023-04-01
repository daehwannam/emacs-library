#!/usr/bin/sh

set -e

BRIGHTNESS=$1  # a value between 0 and 1

for monitor in $(xrandr | grep " connected" | cut -f1 -d " "); do
    xrandr --output $monitor --brightness $BRIGHTNESS
done
