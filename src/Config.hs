module Config ( pureConfig
              ) where

import           Data.Monoid               (All, (<>))

import           Data.Default              (def)
import           Graphics.X11.Types        (Window, mod4Mask)
import           Graphics.X11.Xlib.Extras  (Event)
import           XMonad.Core               (ManageHook, X, XConfig, spawn)
import qualified XMonad.Core               as XC (XConfig (..))
import           XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import           XMonad.Hooks.ManageDocks  (docksEventHook, docksStartupHook,
                                            manageDocks)

import           Keys                      (keys, mouseBindings)
import           Solarized
import           Workspaces                (workspaces)

pureConfig :: a Window -> XConfig a
pureConfig l = def { XC.modMask            = mod4Mask
                   , XC.terminal           = "urxvt"
                   , XC.borderWidth        = 3
                   , XC.normalBorderColor  = base01
                   , XC.focusedBorderColor = orange
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
  spawn $ "xsetroot -solid \"" ++ base0 ++ "\""
  docksStartupHook

