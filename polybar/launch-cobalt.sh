#!/usr/bin/env bash

CONFIG_FILE="$HOME/Linux/xmonad/polybar/config"
killall -qu polybar
polybar --config=$CONFIG_FILE tablet &
