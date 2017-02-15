module Keys ( navigation2DConfig
            , keys
            , mouseBindings
            ) where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Bits                   ((.|.))
import           Data.Default                (def)
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           System.Exit                 (exitSuccess)

import           Graphics.X11.Types          (Button, KeyMask, KeySym, Window,
                                              button1, button2, button3,
                                              controlMask, shiftMask, xK_0,
                                              xK_1, xK_9, xK_Return, xK_Tab,
                                              xK_b, xK_c, xK_comma, xK_e, xK_h,
                                              xK_j, xK_k, xK_l, xK_m, xK_m,
                                              xK_n, xK_p, xK_period, xK_q, xK_r,
                                              xK_space, xK_t, xK_w)
import           XMonad.Actions.Navigation2D (Navigation2DConfig,
                                              centerNavigation,
                                              defaultTiledNavigation,
                                              fullScreenRect, hybridNavigation,
                                              layoutNavigation,
                                              unmappedWindowRect, windowGo,
                                              windowSwap)
import           XMonad.Core                 (Layout, X, XConfig (XConfig),
                                              spawn, whenJust)
import qualified XMonad.Core                 as XC (XConfig (..))
import           XMonad.Hooks.ManageDocks    (ToggleStruts (ToggleStruts))
import           XMonad.Layout               (ChangeLayout (NextLayout),
                                              IncMasterN (IncMasterN),
                                              Resize (Expand, Shrink))
import           XMonad.Operations           (focus, kill, mouseMoveWindow,
                                              mouseResizeWindow, refresh,
                                              restart, screenWorkspace,
                                              sendMessage, setLayout, windows,
                                              withFocused)
import           XMonad.Prompt.Shell         (shellPrompt)
import qualified XMonad.StackSet             as W
import           XMonad.Util.Types           (Direction2D (D, L, R, U))

import           BringWorkspace
import           GridSelectConfig
import           Layouts                     (fullName)
import           PromptConfig
import           Tile                        (MRTMessage (ExpandSlave, ShrinkSlave))
import           Workspaces

sm :: KeyMask
sm = shiftMask

cm :: KeyMask
cm = controlMask

navigation2DConfig :: Navigation2DConfig
navigation2DConfig = def { defaultTiledNavigation = hybridNavigation
                         , layoutNavigation       = [(fullName, centerNavigation)]
                         , unmappedWindowRect     = [(fullName, fullScreenRect)]
                         }

keys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
keys conf@XConfig {XC.modMask = mm} = M.fromList $
    [
      -- quit
      ((mm .|. sm, xK_q     ), liftIO exitSuccess)
      -- restart
    , ((mm,        xK_q     ), restart "xmonad" True)

    -- move focus up or down the window stack
    , ((mm,        xK_Tab   ), windows W.focusDown)
    , ((mm .|. sm, xK_Tab   ), windows W.focusUp)

    -- modifying the window order
    , ((mm,        xK_Return), windows W.swapMaster)

    -- 2D navigation
    , ((mm,        xK_h     ), windowGo L True)
    , ((mm,        xK_j     ), windowGo D True)
    , ((mm,        xK_k     ), windowGo U True)
    , ((mm,        xK_l     ), windowGo R True)

    -- 2D swapping
    , ((mm .|. cm, xK_h     ), windowSwap L False)
    , ((mm .|. cm, xK_j     ), windowSwap D False)
    , ((mm .|. cm, xK_k     ), windowSwap U False)
    , ((mm .|. cm, xK_l     ), windowSwap R False)

    -- resizing the master/slave ratio
    , ((mm .|. sm, xK_h     ), sendMessage Shrink)
    , ((mm .|. sm, xK_j     ), sendMessage ExpandSlave)
    , ((mm .|. sm, xK_k     ), sendMessage ShrinkSlave)
    , ((mm .|. sm, xK_l     ), sendMessage Expand)

    -- increase or decrease number of windows in the master area
    , ((mm,        xK_comma ), sendMessage (IncMasterN 1))
    , ((mm,        xK_period), sendMessage (IncMasterN (-1)))

      -- launch terminal
    , ((mm .|. sm, xK_Return), spawn $ XC.terminal conf)
      -- launch prompt
    , ((mm,        xK_p     ), shellPrompt xpConfig)
      -- launch dmenu
    , ((mm .|. sm, xK_p     ), spawn "dmenu_run")

      -- kill the focused window
    , ((mm .|. sm, xK_c     ), kill)
      -- unfloat the current window
    , ((mm,        xK_t     ), withFocused $ windows . W.sink)

      -- rotate through available layouts
    , ((mm,        xK_space ), sendMessage NextLayout)
      -- reset the layouts on the current workspace to default
    , ((mm .|. sm, xK_space ), setLayout (XC.layoutHook conf))
      -- resize viewed windows to the correct size
    , ((mm,        xK_n     ), refresh)
      -- toggle struts
    , ((mm,        xK_b     ), sendMessage ToggleStruts)

    -- move window to minimsed workspace
    , ((mm,        xK_m     ), withFocused
                               $ windows . W.shiftWin minimisedWS)
    , ((mm .|. sm, xK_m     ), bringWorkspaceWindow minimisedWS gsConfig)

    ]
    ++
    -- mod-[1..9] - switch to workspace N
    -- mod-shift-[1..9] - move client to workspace N
    [((mm .|. m, k), windows $ f i)
        | (i, k) <- zip (XC.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} - switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} - move client to screen 1, 2, or 3
    [((mm .|. m, key), screenWorkspace sc >>= flip whenJust (windows . f))
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
