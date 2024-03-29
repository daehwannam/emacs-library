#!/bin/bash
#
# Features for capturing the screen as image or video.
#
# This code is written by Matt Duck:
# - https://www.mattduck.com/2021-06-exwm-screenshots.html
# - https://github.com/mattduck
#
# Dependency:
# - slop
#   $ sudo apt install slop
# - A compositor, such as "picom"
#   $ sudo apt install picom  # https://github.com/naelstrof/maim/issues/183#issuecomment-512973773
#   - Without a compositor, the following error occurs: Failed to detect a compositor, OpenGL hardware-acceleration disabled...
#   - picom should be running when recording
#     $ picom

_THIS_DATE="$(date --iso-8601=second)"
# _IMAGE_OUTPUT="/f/inbox/screenshots/${_THIS_DATE}.png"
# _VIDEO_OUTPUT="/f/inbox/screenshots/${_THIS_DATE}.mp4"

_IMAGE_ARCHIVE_DIR=/home/$USER/Pictures/
_VIDEO_ARCHIVE_DIR=/home/$USER/Videos/
mkdir -p $_IMAGE_ARCHIVE_DIR
mkdir -p $_VIDEO_ARCHIVE_DIR

_IMAGE_OUTPUT="$_IMAGE_ARCHIVE_DIR/${_THIS_DATE}.png"
_VIDEO_OUTPUT="$_VIDEO_ARCHIVE_DIR/${_THIS_DATE}.mp4"

function image-selection () {
    maim -s >"$_IMAGE_OUTPUT" && firefox "$_IMAGE_OUTPUT"
}

function video-selection-start () {
    # Use slop to grab screen area
    slop=$(slop -f "%x %y %w %h %g %i") || exit 1
    read -r X Y W H G ID < <(echo "$slop")

    # make the width + height divisble by 2 so ffmpeg doesn't error
    if ! [ $((W%2)) -eq 0 ]; then W=$((W+1)); fi
    if ! [ $((H%2)) -eq 0 ]; then H=$((H+1)); fi

    # start capturing video.
    # We use yuv420p here otherwise it can't be played by Firefox.
    # See https://bugzilla.mozilla.org/show_bug.cgi?id=1368063
    ffmpeg -f x11grab -s "$W"x"$H" -r 60 -i :0.0+"$X","$Y" -vcodec h264 -crf 18 -pix_fmt yuv420p -y "$_VIDEO_OUTPUT"  >> /tmp/ffmpg-record.log 2>&1 &

    # store pid
    echo $! >/tmp/ffmpeg-record.pid
    echo "$_VIDEO_OUTPUT" >/tmp/ffmpeg-record.filename
}

function video-stop() {
   pkill --signal INT --pidfile /tmp/ffmpeg-record.pid && firefox "$(cat /tmp/ffmpeg-record.filename)"
}

case "$1" in
    --image-selection) image-selection;;
    --video-selection-start) video-selection-start;;
    --video-stop) video-stop;;
    *) echo "argument invalid or not provided, exiting." && exit 1
esac
