{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase    #-}

import           Graphics.X11.Types        (mod1Mask, mod4Mask)
import           Network.HostName          (getHostName)
import           Options.Applicative
import           XMonad.Core               (getDirectories)
import           XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Main               (launch)

import           Config                    (pureConfig)
import           Layouts                   (layoutHook)
import           Machines                  (Machine (..))
import           PolybarConfig             (polybar)

data Options = Options { oMachine :: Machine
                       , oXephyr  :: Bool
                       } deriving (Show, Eq)

optionsParserInfo :: Machine -> ParserInfo Options
optionsParserInfo mch = info (options mch <**> helper)
  ( fullDesc <> progDesc "xmonad")

options :: Machine -> Parser Options
options mch = do
  xephyr <- switch (  long "xephyr"
                   <> short 'x'
                   <> help "set modMask to modMask1"
                   )
  machine <- option parseMachine (  long "machine"
                                 <> short 'm'
                                 <> metavar "MACHINE"
                                 <> value mch
                                 <> help "override detected machine"
                                 )
  pure $ Options { oMachine = machine
                 , oXephyr = xephyr
                 }

-- ewmh support enables other windows to activate gracefully
-- (see `emacsclient -n`)
main :: IO ()
main = do
  mch <- determineMachine
  opts <- execParser (optionsParserInfo mch)
  dirs <- getDirectories
  let mm = if oXephyr opts then mod1Mask else mod4Mask
  print opts
  config <- polybar mch (ewmh $ pureConfig (oMachine opts) mm layoutHook)
  launch config dirs

determineMachine :: IO Machine
determineMachine = do
  hn <- getHostName
  pure $ case hn of
    "carbon" -> Carbon
    "cobalt" -> Cobalt
    _        -> Unknown

parseMachine :: ReadM Machine
parseMachine = eitherReader $ \case
    "carbon"  -> Right Carbon
    "cobalt"  -> Right Cobalt
    "unknown" -> Right Unknown
    _         -> Left "Machine must be one of carbon, cobalt, or unknown"
