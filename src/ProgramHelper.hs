{-# LANGUAGE LambdaCase #-}

module ProgramHelper ( oneMonitor
                     , twoMonitors
                     , defaultTerminal
                     , isBrowser
                     , isEmacs
                     , isTerminal
                     , isMutt
                     , runTerminal
                     , quickTerm
                     , launcherManageHook
                     , launchersMap
                     ) where

import           Control.Concurrent         (threadDelay)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Data.Bits                  ((.|.))
import qualified Data.Map                   as M
import           Data.Monoid                ((<>))
import           System.Environment         (lookupEnv)

import           Graphics.X11.Types         (KeyMask, KeySym, controlMask,
                                             noModMask, shiftMask, xK_b, xK_c,
                                             xK_d, xK_e, xK_l, xK_m, xK_n)
import           System.FilePath            (dropFileName, (</>))
import           XMonad.Actions.Submap      (submap)
import           XMonad.Actions.WindowGo    (ifWindow, raiseNext,
                                             raiseNextMaybe)
import           XMonad.Core                (ManageHook, Query, X,
                                             withWindowSet)
import           XMonad.Hooks.ManageHelpers (doRectFloat)
import           XMonad.ManageHook          (className, composeAll, doShift,
                                             idHook, (-->), (=?))
import qualified XMonad.StackSet            as W
import           XMonad.Util.Run            (safeSpawn, seconds, unsafeSpawn)

import           Machines                   (Machine (..))

-- Launcher map

launchers :: Machine -> [(KeySym, Launcher)]
launchers mch = [ (xK_b, browserLauncher mch)
                , (xK_c, calculatorLauncher)
                , (xK_d, dictionaryLauncher)
                , (xK_e, emacsLauncher)
                , (xK_m, muttLauncher mch)
                , (xK_n, ncmpcppLauncher mch)
                ]

simpleLaunchers :: Machine -> [(KeySym, X ())]
simpleLaunchers mch = [ (xK_l, runHLedger mch) ]

additionalLaunchers :: [Launcher]
additionalLaunchers = [ quickTermLauncher ]


-- change screens

oneMonitor :: MonadIO m => m ()
oneMonitor = safeSpawn "/home/sam/Linux/xmonad/bin/one-monitor.sh" []

twoMonitors :: MonadIO m => m ()
twoMonitors = safeSpawn "/home/sam/Linux/xmonad/bin/two-monitors.sh" []

-- Terminal

defaultTerminal :: String
defaultTerminal = "kitty"

isTerminal :: String -> Bool
isTerminal "Termite" = True
isTerminal "URxvt"   = True
isTerminal "kitty"   = True
isTerminal _         = False

runTerminal :: MonadIO m => Machine -> m ()
runTerminal mch = safeSpawn defaultTerminal
  ["--override", "font_size=" ++ kittyTerminalSize mch Normal]

quickTermLauncher :: Launcher
quickTermLauncher = Launcher { launcherCommand = unsafeSpawn $ defaultTerminal ++ " --class=" ++ cls
                             , launcherAction = LaunchOrBring
                             , launcherSecondaryAction = NoAction
                             , launcherTertiaryAction = NoAction
                             , launcherQuery = className =? cls
                             , launcherHook = centreTopFloat
                             }
  where cls = "quickTerm"

quickTerm :: X ()
quickTerm = doLauncherAction (launcherAction quickTermLauncher) quickTermLauncher

-- Browser

browserLauncher :: Machine -> Launcher
browserLauncher mch = Launcher { launcherCommand = runBrowser mch
                               , launcherAction = Launch
                               , launcherSecondaryAction = GotoProgram
                               , launcherTertiaryAction = Other (runPrivateBrowser mch)
                               , launcherQuery = toClassNameQuery isBrowser
                               , launcherHook = idHook
                               }

isBrowser :: String -> Bool
isBrowser "Chromium" = True
isBrowser _          = False

runBrowser :: MonadIO m => Machine -> m ()
runBrowser mch = safeSpawn "chromium" [ "--force-device-scale-factor=" ++ browserScale mch
                                      , "--disk-cache-dir=/tmp/cache/chromium"
                                      ]

runPrivateBrowser :: MonadIO m => Machine -> m ()
runPrivateBrowser mch = safeSpawn "chromium" [ "--force-device-scale-factor" ++ browserScale mch
                                             , "--disk-cache-dir=/tmp/cache/chromium"
                                             , "--incognito"
                                             ]

browserScale :: Machine -> String
browserScale Carbon  = "1.75"
browserScale Cobalt  = "2.5"
browserScale Unknown = "1"

-- Calculator

calculatorLauncher :: Launcher
calculatorLauncher = Launcher { launcherCommand = safeSpawn "qalculate-gtk" []
                              , launcherAction = LaunchOrBring
                              , launcherSecondaryAction = NoAction
                              , launcherTertiaryAction = NoAction
                              , launcherQuery = className =? "Qalculate-gtk"
                              , launcherHook = centreFloat
                              }

-- Dictionary

dictionaryLauncher :: Launcher
dictionaryLauncher = Launcher { launcherCommand = safeSpawn "goldendict" []
                              , launcherAction = LaunchOrBring
                              , launcherSecondaryAction = NoAction
                              , launcherTertiaryAction = NoAction
                              , launcherQuery = className =? "GoldenDict"
                              , launcherHook = centreFloat
                              }

-- Emacs

emacsLauncher :: Launcher
emacsLauncher = Launcher { launcherCommand = runEmacs
                         , launcherAction = Launch
                         , launcherSecondaryAction = GotoProgram
                         , launcherTertiaryAction = NoAction
                         , launcherQuery = toClassNameQuery isEmacs
                         , launcherHook = idHook
                         }

isEmacs :: String -> Bool
isEmacs "Emacs" = True
isEmacs _       = False

runEmacs :: MonadIO m => m ()
runEmacs = safeSpawn "emacsclient" ["--create-frame"]


-- Mutt

muttLauncher :: Machine -> Launcher
muttLauncher mch = Launcher { launcherCommand = runMutt mch
                            , launcherAction = LaunchOrGoto
                            , launcherSecondaryAction = LaunchOrBring
                            , launcherTertiaryAction = NoAction
                            , launcherQuery = toClassNameQuery isMutt
                            , launcherHook = idHook
                            }

isMutt :: String -> Bool
isMutt "Mutt" = True
isMutt _      = False

runMutt :: MonadIO m => Machine -> m ()
runMutt mch = runInTerminalWithClass mch Small "Mutt" "neomutt"

-- ncmpcpp

ncmpcppLauncher :: Machine -> Launcher
ncmpcppLauncher mch = Launcher { launcherCommand = runNCMPCpp mch
                               , launcherAction = LaunchOrGoto
                               , launcherSecondaryAction = LaunchOrBring
                               , launcherTertiaryAction = NoAction
                               , launcherQuery = toClassNameQuery isNCMPCpp
                               , launcherHook = idHook
                               }

isNCMPCpp :: String -> Bool
isNCMPCpp "NCMPCpp" = True
isNCMPCpp _         = False

runNCMPCpp :: MonadIO m => Machine -> m ()
runNCMPCpp mch = runInTerminalWithClass mch Normal "NCMPCpp" "ncmpcpp"


-- hledger

runHLedger :: MonadIO m => Machine -> m ()
runHLedger mch = liftIO (lookupEnv "LEDGER_FILE") >>= \case
  Nothing -> pure ()
  Just ledgerFile -> let ledgerDir = dropFileName ledgerFile in do
    openInEmacs $ ledgerDir </> "pending.ledger"
    liftIO $ threadDelay delay
    openInEmacs $ ledgerDir </> "main.ledger"
    liftIO $ threadDelay delay
    runInTerminalWithClass mch Small "" "hledger-statement --watch"
    liftIO $ threadDelay delay
    runInTerminalWithClass mch Small "" "hledger-iadd"
  where delay = seconds 0.2

-- Helpers

toClassNameQuery :: (String -> Bool) -> Query Bool
toClassNameQuery f = fmap f className

runInTerminalWithClass :: MonadIO m => Machine -> TerminalSize -> String -> String -> m ()
runInTerminalWithClass mch size cls cmd=
  unsafeSpawn $ defaultTerminal
                ++ " --override font_size=" ++ kittyTerminalSize mch size
                ++ " --class=" ++ cls
                ++ " zsh -ic '" ++ cmd ++ "'"

data TerminalSize = Small | Normal

kittyTerminalSize :: Machine -> TerminalSize -> String
kittyTerminalSize Carbon Small   = "9"
kittyTerminalSize Carbon Normal  = "12"
kittyTerminalSize Cobalt Small   = "12"
kittyTerminalSize Cobalt Normal  = "15"
kittyTerminalSize Unknown Small  = "9"
kittyTerminalSize Unknown Normal = "12"

openInEmacs :: MonadIO m => FilePath -> m ()
openInEmacs fp = safeSpawn "emacsclient" ["--create-frame", fp]

centreFloat :: ManageHook
centreFloat = doRectFloat $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

centreTopFloat :: ManageHook
centreTopFloat = doRectFloat $ W.RationalRect (1/6) (1/36) (2/3) (5/6)

-- Launcher

data LauncherActions = Launch
                     | GotoProgram
                     | LaunchOrGoto
                     | LaunchOrBring
                     | Other (X ())
                     | NoAction

data Launcher = Launcher { launcherCommand         :: X ()
                         , launcherAction          :: LauncherActions
                         , launcherSecondaryAction :: LauncherActions
                         , launcherTertiaryAction  :: LauncherActions
                         , launcherQuery           :: Query Bool
                         , launcherHook            :: ManageHook
                         }

doLauncherAction :: LauncherActions -> Launcher -> X ()
doLauncherAction Launch l = launcherCommand l
doLauncherAction GotoProgram l = raiseNext (launcherQuery l)
doLauncherAction LaunchOrGoto l = raiseNextMaybe (launcherCommand l) (launcherQuery l)
doLauncherAction LaunchOrBring l =
  do ws <- withWindowSet $ pure . W.currentTag
     let mh = doShift ws <> launcherHook l
     ifWindow (launcherQuery l) mh (launcherCommand l)
doLauncherAction (Other x) _ = x
doLauncherAction NoAction _ = pure ()

launcherManageHook :: Machine -> ManageHook
launcherManageHook mch = composeAll $ mh <$> (additionalLaunchers ++ (snd <$> launchers mch))
  where mh l = launcherQuery l --> launcherHook l

launchersMap :: Machine -> KeyMask -> X ()
launchersMap mch mm = submap $ M.fromList $ concatMap keys (launchers mch) ++ concatMap simpleKeys (simpleLaunchers mch)
  where keys (ks, l) = [ ((noModMask, ks), primaryAction l)
                       , ((mm       , ks), primaryAction l)
                       , ((noModMask .|. shiftMask, ks), secondaryAction l)
                       , ((mm        .|. shiftMask, ks), secondaryAction l)
                       , ((noModMask .|. controlMask, ks), tertiaryAction l)
                       , ((mm        .|. controlMask, ks), tertiaryAction l)
                       ]
        primaryAction l = doLauncherAction (launcherAction l) l
        secondaryAction l = doLauncherAction (launcherSecondaryAction l) l
        tertiaryAction l = doLauncherAction (launcherTertiaryAction l) l
        simpleKeys (ks, x) = [ ((noModMask, ks), x)
                             , ((mm       , ks), x)
                             ]
