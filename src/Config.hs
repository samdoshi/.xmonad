{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Config ( pureConfig
              ) where

import           Data.Monoid                   (All)
import           System.Environment            (setEnv)

import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Reader          (ask)
import           Data.Default                  (def)
import           Graphics.X11.Types            (KeyMask, Window)
import           Graphics.X11.Xlib.Extras      (Event)
import           XMonad.Actions.Navigation2D   (withNavigation2DConfig)
import           XMonad.Core                   (LayoutClass, ManageHook, X,
                                                XConfig)
import qualified XMonad.Core                   as XC (XConfig (..))
import           XMonad.Hooks.InsertPosition   (Focus (Newer),
                                                Position (Below, End, Master),
                                                insertPosition)
import           XMonad.Hooks.ManageDocks      (docksEventHook,
                                                docksStartupHook, manageDocks)
import           XMonad.Hooks.ManageHelpers    (composeOne, currentWs,
                                                doCenterFloat, isDialog, (-?>))
import           XMonad.Hooks.UrgencyHook      (NoUrgencyHook (NoUrgencyHook),
                                                RemindWhen (Dont),
                                                SuppressWhen (Focused),
                                                UrgencyConfig (UrgencyConfig),
                                                withUrgencyHookC)
import           XMonad.Hooks.WindowSwallowing (swallowEventHook)
import           XMonad.Layout.Fullscreen      (fullscreenEventHook,
                                                fullscreenManageHook)
import           XMonad.ManageHook             (className, composeAll, doF,
                                                (=?))
import           XMonad.StackSet               (sink)
import           XMonad.Util.Run               (safeSpawn)

import           Keys                          (keyBindings, mouseBindings,
                                                navigation2DConfig)
import           Machines                      (Machine)
import           ProgramHelper
import           Theme
import           Workspaces

pureConfig :: LayoutClass a Window => Machine -> KeyMask -> (Machine -> a Window) -> XConfig a
pureConfig mch mm l = withNavigation2DConfig navigation2DConfig $
                      withUrgencyHookC NoUrgencyHook (UrgencyConfig Focused Dont) $
                      def { XC.modMask            = mm
                          , XC.terminal           = defaultTerminal
                          , XC.borderWidth        = 6
                          , XC.normalBorderColor  = inactive
                          , XC.focusedBorderColor = active
                          , XC.focusFollowsMouse  = False
                          , XC.clickJustFocuses   = False
                          , XC.workspaces         = workspaces mch
                          , XC.handleEventHook    = handleEventHook mch
                          , XC.logHook            = logHook mch
                          , XC.manageHook         = manageHook mch
                          , XC.layoutHook         = l mch
                          , XC.startupHook        = startupHook mch
                          , XC.keys               = keyBindings mch
                          , XC.mouseBindings      = mouseBindings mch
                          }

handleEventHook :: Machine -> Event -> X All
handleEventHook _ = fullscreenEventHook -- use XMonad.Layout.Fullscreen instead
                                        -- of XMonad.Hooks.EwmhDesktops
                    <> docksEventHook   -- make xmobar (et al.) appear immediately
                    <> swallowEventHook (className =? "kitty") (return True)

-- | My ManageHook
--
-- n.b. hooks are processed bottom to top!
manageHook :: Machine -> ManageHook
manageHook mch = composeAll
  [ manageDocks
  , launcherManageHook mch
  , fullscreenManageHook
  , composeOne [ className =? "Pinentry" -?> doCenterFloat
               , isDialog -?> doCenterFloat
               , currentWs =? homeWS -?> insertPosition End Newer
               , currentWs =? mediaWS -?> insertPosition Master Newer
               , Just <$> insertPosition Below Newer
               ]
  -- things that are allowed to go fullscreen at startup
  --, className =? "Kodi" --> fullscreenManageHook
  , unfloat  -- unfloat everything
  ]
  where unfloat = ask >>= doF . sink

logHook :: Machine -> X ()
logHook _ = pure ()

startupHook :: Machine -> X ()
startupHook _ = do
  liftIO $ setEnv "_JAVA_AWT_WM_NONREPARENTING" "1" -- fix Java (e.g. Arduino)
  safeSpawn "hsetroot" ["-solid", base0]
  docksStartupHook
