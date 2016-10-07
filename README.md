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

If XMonad does not find a `~/.xmonad/xmonad.hs` file it will not attempt to recompile. Instead it will use it's current config.
