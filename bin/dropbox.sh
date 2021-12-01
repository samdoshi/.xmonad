#!/usr/bin/env bash
dropbox=$(maestral status | grep Status)

if [[ $dropbox != "Status:       Up to date" ]]; then
    echo ''
else
    echo ''
fi
