import           Control.Monad.IO.Class       (liftIO)
import           Data.Bits                    ((.|.))
import qualified Data.Map                     as M
import           Data.Monoid                  (All, (<>))
import           System.Exit                  (exitSuccess)

import           Data.Default                 (def)
import           Graphics.X11.Types           (Button, KeyMask, KeySym, Window,
                                               button1, button2, button3,
                                               mod4Mask, shiftMask, xK_1, xK_9,
                                               xK_Return, xK_Tab, xK_c,
                                               xK_comma, xK_e, xK_h, xK_j, xK_k,
                                               xK_l, xK_m, xK_n, xK_p,
                                               xK_period, xK_q, xK_r, xK_space,
                                               xK_t, xK_w)
import           Graphics.X11.Xlib.Extras     (Event)
import           Graphics.X11.Xlib.Types      (Dimension)
import           XMonad.Core                  (Layout, ManageHook, WorkspaceId,
                                               X, XConfig (XConfig), spawn,
                                               whenJust)
import qualified XMonad.Core                  as XC (XConfig (..))
import           XMonad.Hooks.DynamicLog      (xmobar)
import           XMonad.Hooks.EwmhDesktops    (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks     (docksEventHook)
import           XMonad.Layout                (ChangeLayout (NextLayout),
                                               Choose, Full (Full),
                                               IncMasterN (IncMasterN),
                                               Mirror (Mirror),
                                               Resize (Expand, Shrink),
                                               Tall (Tall), (|||))
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Layout.Spacing        (SmartSpacing, smartSpacing)
import           XMonad.Main                  (xmonad)
import           XMonad.Operations            (focus, kill, mouseMoveWindow,
                                               mouseResizeWindow, refresh,
                                               screenWorkspace, sendMessage,
                                               setLayout, windows, withFocused)
import           XMonad.Prompt                (XPConfig, XPPosition (Top))
import qualified XMonad.Prompt                as XP (XPConfig (..))
import           XMonad.Prompt.Shell          (shellPrompt)
import qualified XMonad.StackSet              as W

import           Solarized

workspaces :: [WorkspaceId]
workspaces = ["1", "2", "3"]

modMask :: KeyMask
modMask = mod4Mask

terminal :: String
terminal = "urxvt"

borderWidth :: Dimension
borderWidth = 2

spacingWidth :: Int
spacingWidth = 3

normalBorderColour :: String
normalBorderColour = base01

focusedBorderColour :: String
focusedBorderColour = red

type TallLayout = ModifiedLayout SmartSpacing Tall
type LayoutHook = Choose TallLayout (Choose (Mirror TallLayout) Full)

layoutHook :: LayoutHook a
layoutHook = tiled ||| Mirror tiled ||| Full
  where
     tiled = smartSpacing spacingWidth $ Tall 1 (2/100) (1/2)

eventHook :: Event -> X All
eventHook = fullscreenEventHook -- extra hook to get chrome to work
                                -- not included in ewmh
            <> docksEventHook   -- make xmobar (et al.) appear immediately

manageHook :: ManageHook
manageHook = def

logHook :: X ()
logHook = pure ()

keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys conf@XConfig {XC.modMask = mm} = M.fromList $
    [
      -- launch terminal
      ((mm .|. shiftMask, xK_Return), spawn $ XC.terminal conf)
      -- launch dmenu
    , ((mm, xK_p                   ), shellPrompt myXPConfig)
    , ((mm .|. shiftMask, xK_p     ), spawn "dmenu_run")

      -- kill the focused window
    , ((mm .|. shiftMask, xK_c     ), kill)
      -- unfloat the current window
    , ((mm,               xK_t     ), withFocused $ windows . W.sink)

      -- rotate through available layouts
    , ((mm,               xK_space ), sendMessage NextLayout)
      -- reset the layouts on the current workspace to default
    , ((mm .|. shiftMask, xK_space ), setLayout $ XC.layoutHook conf)
      -- resize viewed windows to the correct size
    , ((mm,               xK_n     ), refresh)

    -- move focus up or down the window stack
    , ((mm,               xK_Tab   ), windows W.focusDown)
    , ((mm .|. shiftMask, xK_Tab   ), windows W.focusUp)
    , ((mm,               xK_j     ), windows W.focusDown)
    , ((mm,               xK_k     ), windows W.focusUp)
    , ((mm,               xK_m     ), windows W.focusMaster)

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
        | (i, k) <- zip (XC.workspaces conf) [xK_1 .. xK_9]
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
    [ ((mm, button1), \w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster)
    -- raise the window to the top of the stack
    , ((mm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    -- set the window to floating mode and resize by dragging
    , ((mm, button3), \w -> focus w >> mouseResizeWindow w
                                         >> windows W.shiftMaster)
    ]

myXPConfig :: XPConfig
myXPConfig = def { XP.font = "xft:Roboto Mono:size=12"
                 , XP.bgColor = base1
                 , XP.fgColor = base03
                 , XP.fgHLight = base3
                 , XP.bgHLight = base1
                 , XP.borderColor = base03
                 , XP.position = Top
                 , XP.height = 30
                 }

-- ewmh support enables other windows to activate gracefully
-- (see `emacsclient -n`)
myConfig :: XConfig LayoutHook
myConfig = ewmh def { XC.modMask = modMask
                    , XC.terminal = terminal
                    , XC.borderWidth = borderWidth
                    , XC.normalBorderColor = normalBorderColour
                    , XC.focusedBorderColor = focusedBorderColour
                    , XC.workspaces = workspaces
                    , XC.handleEventHook = eventHook
                    , XC.logHook = logHook
                    , XC.manageHook = manageHook
                    , XC.layoutHook = layoutHook
                    , XC.keys = keys
                    , XC.mouseBindings = mouseBindings
                    }

main :: IO ()
main = xmonad =<< xmobar myConfig
