#!/bin/bash
count=$(ls ~/Linux/mail/fastmail/INBOX/new | wc -l)
if [[ $count != "0" ]]; then
    echo -ne ''
fi
