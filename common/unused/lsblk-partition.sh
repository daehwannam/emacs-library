#!/usr/bin/env bash

lsblk -o name,size,uuid,mountpoint | \
    grep -v loop | grep '^\(├─\|└─\)' | \
    awk '{printf "%s|%s|%s|%s\n",$1,$2,$3,$4}'
