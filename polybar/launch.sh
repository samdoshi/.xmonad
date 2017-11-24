#!/bin/bash

killall -qu polybar
polybar --reload --config="$HOME/.xmonad/polybar/config" left &
polybar --reload --config="$HOME/.xmonad/polybar/config" right &
