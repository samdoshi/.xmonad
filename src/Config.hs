{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Config ( pureConfig
              ) where

import           Data.Monoid                 (All, (<>))
import           System.Environment          (setEnv)

import           Control.Monad.IO.Class      (liftIO)
import           Data.Default                (def)
import           Graphics.X11.Types          (Window, mod4Mask)
import           Graphics.X11.Xlib.Cursor    (xC_left_ptr)
import           Graphics.X11.Xlib.Extras    (Event)
import           XMonad.Actions.Navigation2D (withNavigation2DConfig)
import           XMonad.Core                 (LayoutClass, ManageHook, X,
                                              XConfig)
import qualified XMonad.Core                 as XC (XConfig (..))
import           XMonad.Hooks.EwmhDesktops   (fullscreenEventHook)
import           XMonad.Hooks.ManageDocks    (docksEventHook, docksStartupHook,
                                              manageDocks)
import           XMonad.Hooks.UrgencyHook    (NoUrgencyHook (NoUrgencyHook),
                                              RemindWhen (Dont),
                                              SuppressWhen (Focused),
                                              UrgencyConfig (UrgencyConfig),
                                              withUrgencyHookC)
import           XMonad.Util.Cursor          (setDefaultCursor)
import           XMonad.Util.Run             (safeSpawn)

import           Keys                        (keys, mouseBindings,
                                              navigation2DConfig)
import           Solarized
import           Workspaces                  (workspaces)

pureConfig :: LayoutClass a Window => a Window -> XConfig a
pureConfig l = withNavigation2DConfig navigation2DConfig $
               withUrgencyHookC NoUrgencyHook (UrgencyConfig Focused Dont) $
               def { XC.modMask            = mod4Mask
                   , XC.terminal           = "urxvt"
                   , XC.borderWidth        = 2
                   , XC.normalBorderColor  = inactive
                   , XC.focusedBorderColor = active
                   , XC.focusFollowsMouse  = False
                   , XC.clickJustFocuses   = True
                   , XC.workspaces         = workspaces
                   , XC.handleEventHook    = handleEventHook
                   , XC.logHook            = logHook
                   , XC.manageHook         = manageHook
                   , XC.layoutHook         = l
                   , XC.startupHook        = startupHook
                   , XC.keys               = keys
                   , XC.mouseBindings      = mouseBindings
                   }

handleEventHook :: Event -> X All
handleEventHook = fullscreenEventHook -- extra hook to get chrome to work
                                      -- not included in ewmh
                  <> docksEventHook   -- make xmobar (et al.) appear immediately

manageHook :: ManageHook
manageHook = manageDocks

logHook :: X ()
logHook = pure ()

startupHook :: X ()
startupHook = do
  liftIO $ setEnv "_JAVA_AWT_WM_NONREPARENTING" "1" -- fix Java (e.g. Arduino)
  safeSpawn "xsetroot" ["-solid", base0]
  setDefaultCursor xC_left_ptr
  docksStartupHook

