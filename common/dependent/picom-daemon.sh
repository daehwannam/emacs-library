#!/bin/bash

if ! pgrep picom > /dev/null 2>&1
then
    nohup picom > /dev/null 2>&1 &
fi
