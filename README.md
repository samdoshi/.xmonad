# xmonad-samdoshi

## Installation

```
git clone git@github.com:samdoshi/.xmonad.git ~/.xmonad
cd ~/.xmonad
make install
```

Add `exec xmonad` to `.xinitrc`.

This will install the `xmonad` binary to `~/.local/bin`, that version must be first in your `$PATH` for `mod-Q` to work.

## Disabling XMonad recompilation

By using `XMonad.Main.launch` we can disable the built-in recompilation support, we also need to use `XMonad.Operations.restart` to restart.
