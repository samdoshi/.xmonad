#!/usr/bin/env bash

MONITOR_COUNT=$(xrandr | awk '/ connected/ && /[[:digit:]]x[[:digit:]].*+/{print $1}' | wc -l)

CONFIG_FILE="$HOME/Linux/xmonad/polybar/config"

killall -qu polybar

if ((MONITOR_COUNT <= 1)); then
    echo One
    polybar --config=$CONFIG_FILE one &
else
    echo Multiple
    polybar --config=$CONFIG_FILE left &
    polybar --config=$CONFIG_FILE right &
fi;
