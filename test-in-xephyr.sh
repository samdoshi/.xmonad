#!/usr/bin/env bash

set -e

Xephyr -br -ac -noreset -screen 3000x2000 :1 &
XEPHYR_PID=$!

(
    sleep 1
    DISPLAY=:1 cabal new-exec xmonad -- --xephyr &
    XMONAD_PID=$1
    wait $XMONAD_PID
    kill $XEPHYR_PID
)
