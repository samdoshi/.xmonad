#!/bin/bash
count=$(ls ~/Linux/mail/fastmail/INBOX/new | wc -l)
if [[ $count != "0" ]]; then
    echo -e ' <fc=#cb4b16><fn=2>\uf0e0</fn></fc> '
fi
