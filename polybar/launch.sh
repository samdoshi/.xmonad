#!/bin/bash

MONITOR_COUNT=$(xrandr | awk '/\ connected/ && /[[:digit:]]x[[:digit:]].*+/{print $1}' | wc -l)

killall -qu polybar

if ((MONITOR_COUNT <= 1)); then
    echo One
    polybar --config="$HOME/.xmonad/polybar/config" one &
else
    echo Multiple
    polybar --config="$HOME/.xmonad/polybar/config" left &
    polybar --config="$HOME/.xmonad/polybar/config" right &
fi;
