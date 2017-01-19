import           XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Main               (launch)

import           Config                    (pureConfig)
import           Layouts                   (layoutHook)
import           XmobarConfig              (xmobar)

-- ewmh support enables other windows to activate gracefully
-- (see `emacsclient -n`)
main :: IO ()
main = launch =<< xmobar (ewmh $ pureConfig layoutHook)
