import           Data.Maybe                (fromMaybe)
import           System.Environment        (getArgs)
import           Text.Read                 (readMaybe)

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

-- TODO change this to get machine from hostname
determineMachine :: IO Machine
determineMachine = do
  args <- getArgs
  pure $ case args of
    [s] -> fromMaybe Unknown (readMaybe s)
    _   -> Unknown
