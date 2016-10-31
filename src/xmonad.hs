{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Monad.IO.Class       (liftIO)
import           Data.Bits                    ((.|.))
import qualified Data.Map                     as M
import           Data.Monoid                  (All, (<>))
import           System.Exit                  (exitSuccess)

import           Data.Default                 (def)
import           Graphics.X11.Types           (Button, KeyMask, KeySym, Window,
                                               button1, button2, button3,
                                               mod4Mask, shiftMask, xK_0, xK_1,
                                               xK_9, xK_Return, xK_Tab, xK_b,
                                               xK_c, xK_comma, xK_e, xK_h, xK_j,
                                               xK_k, xK_l, xK_m, xK_m, xK_n,
                                               xK_p, xK_period, xK_q, xK_r,
                                               xK_space, xK_t, xK_w)
import           Graphics.X11.Xlib.Extras     (Event)
import           Graphics.X11.Xlib.Types      (Dimension)
import           XMonad.Actions.GridSelect    (GSConfig (..), HasColorizer)
import           XMonad.Core                  (Layout, ManageHook, WorkspaceId,
                                               X, XConfig (XConfig), spawn,
                                               whenJust)
import qualified XMonad.Core                  as XC (XConfig (..))
import           XMonad.Hooks.DynamicLog      (PP (..), dynamicLogWithPP,
                                               xmobarColor)
import           XMonad.Hooks.EwmhDesktops    (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks     (AvoidStruts,
                                               ToggleStruts (ToggleStruts),
                                               avoidStruts, docksEventHook,
                                               docksStartupHook, manageDocks)
import           XMonad.Layout                (ChangeLayout (NextLayout),
                                               Choose, Full (Full),
                                               IncMasterN (IncMasterN),
                                               Resize (Expand, Shrink),
                                               Tall (Tall), (|||))
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Layout.NoBorders      (SmartBorder, smartBorders)
import           XMonad.Layout.Renamed        (Rename (Replace), renamed)
import           XMonad.Layout.Spacing        (Spacing, spacing)
import           XMonad.Main                  (xmonad)
import           XMonad.Operations            (focus, kill, mouseMoveWindow,
                                               mouseResizeWindow, refresh,
                                               screenWorkspace, sendMessage,
                                               setLayout, windows, withFocused)
import           XMonad.Prompt                (XPConfig,
                                               XPPosition (CenteredAt))
import qualified XMonad.Prompt                as XP (XPConfig (..))
import           XMonad.Prompt.Shell          (shellPrompt)
import qualified XMonad.StackSet              as W
import           XMonad.Util.Run              (hPutStrLn, spawnPipe)

import           BringWorkspace
import           Solarized

homeWS :: WorkspaceId
homeWS = "home"

alphaWS :: WorkspaceId
alphaWS = "alpha"

betaWS :: WorkspaceId
betaWS = "beta"

mediaWS :: WorkspaceId
mediaWS = "media"

gamesWS :: WorkspaceId
gamesWS = "games"

floatWS :: WorkspaceId
floatWS = "float"

minimisedWS :: WorkspaceId
minimisedWS = "minimised"

workspaces :: [WorkspaceId]
workspaces = [ homeWS, alphaWS, betaWS, mediaWS, floatWS, gamesWS
             , "7", "8", "9"
             , minimisedWS
             ]

modMask :: KeyMask
modMask = mod4Mask

terminal :: String
terminal = "urxvt"

borderWidth :: Dimension
borderWidth = 3

spacingWidth :: Int
spacingWidth = 3

normalBorderColour :: String
normalBorderColour = base01

focusedBorderColour :: String
focusedBorderColour = orange

type ML = ModifiedLayout
type TallLayout = ML Rename (ML AvoidStruts (ML Spacing Tall))
type FullLayout = ML SmartBorder Full
type ChooseLayout = Choose TallLayout FullLayout
type LayoutHook = ChooseLayout

layoutHook :: LayoutHook Window
layoutHook = tall' ||| smartBorders Full
  where tall = avoidStruts $ spacing spacingWidth $ Tall 1 (2/100) (1/2)
        tall' = renamed [Replace "Tall"] tall

eventHook :: Event -> X All
eventHook = fullscreenEventHook -- extra hook to get chrome to work
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

keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys conf@XConfig {XC.modMask = mm} = M.fromList $
    [
      -- launch terminal
      ((mm .|. shiftMask, xK_Return), spawn $ XC.terminal conf)
      -- launch dmenu
    , ((mm,               xK_p     ), shellPrompt xpConfig)
    , ((mm .|. shiftMask, xK_p     ), spawn "dmenu_run")

      -- kill the focused window
    , ((mm .|. shiftMask, xK_c     ), kill)
      -- unfloat the current window
    , ((mm,               xK_t     ), withFocused $ windows . W.sink)

      -- rotate through available layouts
    , ((mm,               xK_space ), sendMessage NextLayout)
      -- reset the layouts on the current workspace to default
    , ((mm .|. shiftMask, xK_space ), setLayout (XC.layoutHook conf)
                                      >> docksStartupHook) -- find docks again
      -- resize viewed windows to the correct size
    , ((mm,               xK_n     ), refresh)
      -- toggle struts
    , ((mm,               xK_b     ), sendMessage ToggleStruts)

    -- move focus up or down the window stack
    , ((mm,               xK_Tab   ), windows W.focusDown)
    , ((mm .|. shiftMask, xK_Tab   ), windows W.focusUp)
    , ((mm,               xK_j     ), windows W.focusDown)
    , ((mm,               xK_k     ), windows W.focusUp)

    -- move window to minimsed workspace
    , ((mm,               xK_m     ), withFocused
                                      $ windows . W.shiftWin minimisedWS)
    , ((mm .|. shiftMask, xK_m     ), bringWorkspaceWindow minimisedWS gsConfig)

    -- modifying the window order
    , ((mm,               xK_Return), windows W.swapMaster)
    , ((mm .|. shiftMask, xK_j     ), windows W.swapDown)
    , ((mm .|. shiftMask, xK_k     ), windows W.swapUp)

    -- resizing the master/slave ratio
    , ((mm,               xK_h     ), sendMessage Shrink)
    , ((mm,               xK_l     ), sendMessage Expand)


    -- increase or decrease number of windows in the master area
    , ((mm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((mm              , xK_period), sendMessage (IncMasterN (-1)))

    -- quit, or restart
    , ((mm .|. shiftMask, xK_q     ), liftIO exitSuccess)
    , ((mm              , xK_q     ), spawn "xmonad --restart")
    ]
    ++
    -- mod-[1..9] - switch to workspace N
    -- mod-shift-[1..9] - move client to workspace N
    [((m .|. mm, k), windows $ f i)
        | (i, k) <- zip (XC.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} - switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} - move client to screen 1, 2, or 3
    [((m .|. mm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

mouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings XConfig {XC.modMask = mm} = M.fromList
    -- set the window to floating mode and move by dragging
    [ ((mm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    -- raise the window to the top of the stack
    , ((mm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    -- set the window to floating mode and resize by dragging
    , ((mm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

xpConfig :: XPConfig
xpConfig = def { XP.font = "xft:Roboto Mono:size=16"
               , XP.bgColor = base02
               , XP.fgColor = base1
               , XP.fgHLight = orange
               , XP.bgHLight = base02
               , XP.borderColor = base01
               , XP.promptBorderWidth = 5
               , XP.position = CenteredAt 0.25 0.5
               , XP.height = 50
               }

gsConfig :: HasColorizer a => GSConfig a
gsConfig = def { gs_cellheight = 40
               , gs_cellwidth = 400
               , gs_cellpadding = 5
               , gs_font = "xft:Roboto Mono:pixelsize=16"
               , gs_originFractX = 1/2
               , gs_originFractY = 1/3
               }

pureConfig :: a Window -> XConfig a
pureConfig l = def { XC.modMask = modMask
                   , XC.terminal = terminal
                   , XC.borderWidth = borderWidth
                   , XC.normalBorderColor = normalBorderColour
                   , XC.focusedBorderColor = focusedBorderColour
                   , XC.workspaces = workspaces
                   , XC.handleEventHook = eventHook
                   , XC.logHook = logHook
                   , XC.manageHook = manageHook
                   , XC.layoutHook = l
                   , XC.startupHook = startupHook
                   , XC.keys = keys
                   , XC.mouseBindings = mouseBindings
                   }


xmobar :: XConfig a -> IO (XConfig a)
xmobar c = do
    h <- spawnPipe "xmobar /home/sam/.xmonad/xmobarrc"
    pure $ c { XC.logHook = XC.logHook c >> dynamicLogWithPP (pp h) }
  where
    pp h = def { ppCurrent         = xmobarColor blue "" . fnBold . awesome
               , ppHidden          = xmobarColor base1 "" . awesome
               , ppHiddenNoWindows = xmobarColor base03 "" . awesome . hideEmpty
               , ppTitle           = xmobarColor blue "" . fnBold . shorten 128
               , ppVisible         = wrap "(" ")" -- Xinerama only
               , ppUrgent          = xmobarColor red yellow
               , ppSep             = "  "
               , ppWsSep           = " "
               , ppOutput          = hPutStrLn h
               }
    awesome x | x == homeWS      = fnAwesome "\xf015"
              | x == alphaWS     = "\x03b1"
              | x == betaWS      = "\x03b2"
              | x == mediaWS     = fnAwesome "\xf03e"
              | x == gamesWS     = fnAwesome "\xf11b"
              | x == floatWS     = "f" -- fnAwesome "\xf2d2" (needs FA 4.7)
              | x == minimisedWS = fnAwesome "\xf00a"
              | otherwise = x
    hideEmpty x | x `elem` ["7", "8", "9"] = ""
                | otherwise                     = x
    fnBold = wrap "<fn=1>" "</fn>"
    fnAwesome = wrap "<fn=2>" "</fn>"
    wrap _ _ "" = ""
    wrap l r m  = l ++ m ++ r
    shorten n xs | length xs < n = xs
                 | otherwise     = take (n - length end) xs ++ end
    end = "..."

-- ewmh support enables other windows to activate gracefully
-- (see `emacsclient -n`)
main :: IO ()
main = xmonad =<< xmobar (ewmh $ pureConfig layoutHook)
