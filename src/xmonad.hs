import           XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Main               (launch)

import           Config                    (pureConfig)
import           Layouts                   (layoutHook)
import           PolybarConfig             (polybar)

-- ewmh support enables other windows to activate gracefully
-- (see `emacsclient -n`)
main :: IO ()
main = launch =<< polybar (ewmh $ pureConfig layoutHook)
