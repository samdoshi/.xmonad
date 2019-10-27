#!/usr/bin/env bash
dropbox=$(dropbox.py status 2>&1)

if [[ $dropbox != "Up to date" ]]; then
    echo ''
else
    echo ''
fi
