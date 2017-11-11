#!/bin/bash
set -e

gpg=$(~/.xmonad/bin/gpg-status.sh)
mail=$(~/.xmonad/bin/new-mail.sh)

echo "$gpg$mail"
