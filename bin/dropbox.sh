#!/usr/bin/env bash
dropbox=$(/home/sam/.local/bin/dropbox.py status 2>&1)

if [[ $dropbox != "Up to date" ]]; then
    echo ''
else
    echo ''
fi
