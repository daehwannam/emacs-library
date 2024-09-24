#!/usr/bin/bash

# https://unix.stackexchange.com/a/119816/502857

export USBKEYS=($(
    xargs -n1 readlink < <(echo /sys/block/*) |
    sed -ne 's+^.*/usb[0-9].*/\([^/]*\)$+/sys/block/\1/device/uevent+p' |
    xargs grep -H ^DRIVER=sd |
    sed s/device.uevent.*$/size/ |
    xargs grep -Hv ^0$ |
    cut -d / -f 4
))
for dev in ${USBKEYS[@]} ;do
    echo $dev \"$(
        sed -e s/\ *$//g </sys/block/$dev/device/model
        )\" ;
  done
