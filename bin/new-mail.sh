#!/usr/bin/env bash
maildir="$HOME/Linux/mail/fastmail/INBOX/new"

if [ -d $maildir ]; then
    count=$(ls $maildir | wc -l)
    if [[ $count != "0" ]]; then
        echo ''
    else
        echo ''
    fi
else
     echo ''
fi
