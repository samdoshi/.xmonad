import           Network.HostName          (getHostName)
import           XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Main               (launch)

import           Config                    (pureConfig)
import           Layouts                   (layoutHook)
import           Machines                  (Machine (..))
import           PolybarConfig             (polybar)

-- ewmh support enables other windows to activate gracefully
-- (see `emacsclient -n`)
main :: IO ()
main = do
  mch <- determineMachine
  launch =<< polybar mch (ewmh $ pureConfig mch layoutHook)

determineMachine :: IO Machine
determineMachine = do
  hn <- getHostName
  pure $ case hn of
    "carbon" -> Carbon
    "cobalt" -> Cobalt
    _        -> Unknown
