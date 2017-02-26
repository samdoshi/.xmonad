#!/bin/bash
set -e

# store cache ids, SIGN_ID, ENCRYPT_ID  in gpg-cache-ids.sh
# obtain ids with `gpg --list-secret-keys --with-keygrip`, the id is the keygrip
source ~/.xmonad/bin/gpg-cache-ids.sh

function gpg_keyinfo() {
    local output=$(gpg-connect-agent "KEYINFO $1" /bye)
    echo $output
}

function gpg_key_status() {
    local keyinfo=$(gpg_keyinfo $1)
    # '1' indicates the passphrase is cached, '-' that it is not
    echo ${keyinfo:57:1}
}

sign_status=$(gpg_key_status $SIGN_ID)
encrypt_status=$(gpg_key_status $ENCRYPT_ID)

if [ $sign_status == "1" ] || [ $encrypt_status == "1" ]; then
    echo -e '<fc=#cb4b16><fn=2>\uf13e</fn></fc>'
else
    echo -e '<fn=2>\uf023</fn>'
fi
