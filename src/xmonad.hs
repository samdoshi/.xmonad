import           XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Main               (xmonad)

import           Config                    (pureConfig)
import           Layouts                   (layoutHook)
import           XmobarConfig              (xmobar)

-- ewmh support enables other windows to activate gracefully
-- (see `emacsclient -n`)
main :: IO ()
main = xmonad =<< xmobar (ewmh $ pureConfig layoutHook)
