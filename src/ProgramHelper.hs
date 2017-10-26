module ProgramHelper ( defaultTerminal
                     , isBrowser
                     , isEmacs
                     , isTerminal
                     , isMutt
                     , runTerminal
                     , launcherManageHook
                     , launchersMap
                     ) where

import           Control.Monad.Trans        (MonadIO)
import           Data.Bits                  ((.|.))
import qualified Data.Map                   as M
import           Data.Monoid                ((<>))

import           Graphics.X11.Types         (KeyMask, KeySym, controlMask,
                                             noModMask, shiftMask, xK_b, xK_c,
                                             xK_d, xK_e, xK_m)
import           XMonad.Actions.Submap      (submap)
import           XMonad.Actions.WindowGo    (ifWindow, raiseNext,
                                             raiseNextMaybe)
import           XMonad.Core                (ManageHook, Query, X,
                                             withWindowSet)
import           XMonad.Hooks.ManageHelpers (doRectFloat)
import           XMonad.ManageHook          (className, composeAll, doShift,
                                             idHook, (-->), (=?))
import qualified XMonad.StackSet            as W
import           XMonad.Util.Run            (safeSpawn, unsafeSpawn)


-- Launcher map

launchers :: [(KeySym, Launcher)]
launchers = [ (xK_b, browserLauncher)
            , (xK_c, calculatorLauncher)
            , (xK_d, dictionaryLauncher)
            , (xK_e, emacsLauncher)
            , (xK_m, muttLauncher)
            ]


-- Terminal

defaultTerminal :: String
defaultTerminal = "termite"

isTerminal :: String -> Bool
isTerminal "Termite" = True
isTerminal "URxvt"   = True
isTerminal _         = False

runTerminal :: MonadIO m => m ()
runTerminal = safeSpawn defaultTerminal []

-- Browser

browserLauncher :: Launcher
browserLauncher = Launcher { launcherCommand = runBrowser
                           , launcherAction = Launch
                           , launcherSecondaryAction = GotoProgram
                           , launcherTertiaryAction = Other runPrivateBrowser
                           , launcherQuery = toClassNameQuery isBrowser
                           , launcherHook = idHook
                           }

isBrowser :: String -> Bool
isBrowser "Chromium" = True
isBrowser _          = False

runBrowser :: MonadIO m => m ()
runBrowser = safeSpawn "chromium" [ "--force-device-scale-factor=1.75"
                                  , "--disk-cache-dir=/tmp/cache/chromium"
                                  ]

runPrivateBrowser :: MonadIO m => m ()
runPrivateBrowser = safeSpawn "chromium" [ "--force-device-scale-factor=1.75"
                                         , "--disk-cache-dir=/tmp/cache/chromium"
                                         , "--incognito"
                                         ]

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

muttLauncher :: Launcher
muttLauncher = Launcher { launcherCommand = runMutt
                        , launcherAction = LaunchOrGoto
                        , launcherSecondaryAction = LaunchOrBring
                        , launcherTertiaryAction = NoAction
                        , launcherQuery = toClassNameQuery isMutt
                        , launcherHook = idHook
                        }

isMutt :: String -> Bool
isMutt "Mutt" = True
isMutt _      = False

runMutt :: MonadIO m => m ()
runMutt = runInTerminalWithClass "neomutt" "Mutt"


-- Helpers

toClassNameQuery :: (String -> Bool) -> Query Bool
toClassNameQuery f = fmap f className

runInTerminalWithClass :: MonadIO m => String -> String -> m ()
runInTerminalWithClass cmd cls =
  unsafeSpawn $ defaultTerminal ++ " --exec=\"zsh -ic " ++ cmd ++ "\" --class=" ++ cls

centreFloat :: ManageHook
centreFloat = doRectFloat $ W.RationalRect (1/6) (1/6) (2/3) (2/3)


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

launcherManageHook :: ManageHook
launcherManageHook = composeAll $ mh . snd <$> launchers
  where mh l = launcherQuery l --> launcherHook l

launchersMap :: KeyMask -> X ()
launchersMap mm = submap $ M.fromList $ concatMap keys launchers
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
