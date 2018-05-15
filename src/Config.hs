{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Config ( pureConfig
              ) where

import           Data.Monoid                 (All, (<>))
import           System.Environment          (setEnv)

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ask)
import           Data.Default                (def)
import           Graphics.X11.Types          (Window, mod4Mask)
import           Graphics.X11.Xlib.Cursor    (xC_left_ptr)
import           Graphics.X11.Xlib.Extras    (Event)
import           XMonad.Actions.Navigation2D (withNavigation2DConfig)
import           XMonad.Core                 (LayoutClass, ManageHook, X,
                                              XConfig)
import qualified XMonad.Core                 as XC (XConfig (..))
import           XMonad.Hooks.InsertPosition (Focus (Newer),
                                              Position (Below, End, Master),
                                              insertPosition)
import           XMonad.Hooks.ManageDocks    (docksEventHook, docksStartupHook,
                                              manageDocks)
import           XMonad.Hooks.ManageHelpers  (composeOne, currentWs,
                                              doCenterFloat, isDialog, (-?>))
import           XMonad.Hooks.UrgencyHook    (NoUrgencyHook (NoUrgencyHook),
                                              RemindWhen (Dont),
                                              SuppressWhen (Focused),
                                              UrgencyConfig (UrgencyConfig),
                                              withUrgencyHookC)
import           XMonad.Layout.Fullscreen    (fullscreenEventHook)
import           XMonad.ManageHook           (className, composeAll, doF, (=?))
import           XMonad.StackSet             (sink)
import           XMonad.Util.Cursor          (setDefaultCursor)
import           XMonad.Util.Run             (safeSpawn)


import           Keys                        (keyBindings, mouseBindings,
                                              navigation2DConfig)
import           ProgramHelper
import           Theme
import           Workspaces

pureConfig :: LayoutClass a Window => a Window -> XConfig a
pureConfig l = withNavigation2DConfig navigation2DConfig $
               withUrgencyHookC NoUrgencyHook (UrgencyConfig Focused Dont) $
               def { XC.modMask            = mod4Mask
                   , XC.terminal           = defaultTerminal
                   , XC.borderWidth        = 6
                   , XC.normalBorderColor  = inactive
                   , XC.focusedBorderColor = active
                   , XC.focusFollowsMouse  = False
                   , XC.clickJustFocuses   = False
                   , XC.workspaces         = workspaces
                   , XC.handleEventHook    = handleEventHook
                   , XC.logHook            = logHook
                   , XC.manageHook         = manageHook
                   , XC.layoutHook         = l
                   , XC.startupHook        = startupHook
                   , XC.keys               = keyBindings
                   , XC.mouseBindings      = mouseBindings
                   }

handleEventHook :: Event -> X All
handleEventHook = fullscreenEventHook -- use XMonad.Layout.Fullscreen instead
                                      -- of XMonad.Hooks.EwmhDesktops
                  <> docksEventHook   -- make xmobar (et al.) appear immediately

-- | My ManageHook
--
-- n.b. hooks are processed bottom to top!
manageHook :: ManageHook
manageHook = composeAll
  [ manageDocks
  , launcherManageHook
  , composeOne [ className =? "Pinentry-gtk-2" -?> doCenterFloat
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

logHook :: X ()
logHook = pure ()

startupHook :: X ()
startupHook = do
  liftIO $ setEnv "_JAVA_AWT_WM_NONREPARENTING" "1" -- fix Java (e.g. Arduino)
  safeSpawn "hsetroot" ["-solid", base0]
  setDefaultCursor xC_left_ptr
  docksStartupHook

