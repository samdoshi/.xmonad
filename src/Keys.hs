module Keys ( navigation2DConfig
            , keyBindings
            , mouseBindings
            ) where

import           Control.Monad.IO.Class             (liftIO)
import           Data.Bits                          ((.|.))
import           Data.Default                       (def)
import           Data.Map                           (Map)
import qualified Data.Map                           as M
import           System.Exit                        (exitSuccess)

import           Graphics.X11.ExtraTypes.XF86       (xF86XK_AudioLowerVolume,
                                                     xF86XK_AudioMute,
                                                     xF86XK_AudioRaiseVolume)
import           Graphics.X11.Types                 (Button, KeyMask, KeySym,
                                                     Window, button1, button2,
                                                     button3, controlMask,
                                                     mod1Mask, noModMask,
                                                     shiftMask, xK_0, xK_1,
                                                     xK_9, xK_BackSpace,
                                                     xK_Return, xK_Tab, xK_a,
                                                     xK_b, xK_comma, xK_equal,
                                                     xK_f, xK_g, xK_grave, xK_h,
                                                     xK_j, xK_k, xK_l, xK_m,
                                                     xK_minus, xK_n, xK_p,
                                                     xK_period, xK_q, xK_s,
                                                     xK_space, xK_t)
import           XMonad.Actions.CopyWindow          (copy, kill1)
import           XMonad.Actions.GridSelect          (bringSelected,
                                                     goToSelected)
import           XMonad.Actions.MessageFeedback     (sendSomeMessageB,
                                                     tryMessageB)
import           XMonad.Actions.Navigation2D        (Navigation2DConfig,
                                                     centerNavigation,
                                                     centerNavigation,
                                                     defaultTiledNavigation,
                                                     fullScreenRect, hybridOf,
                                                     layoutNavigation,
                                                     sideNavigationWithBias,
                                                     unmappedWindowRect,
                                                     windowGo, windowSwap)
import           XMonad.Core                        (Layout, Message, X,
                                                     XConfig (XConfig))
import qualified XMonad.Core                        as XC (XConfig (..))
import           XMonad.Hooks.ManageDocks           (ToggleStruts (ToggleStruts))
import           XMonad.Layout                      (ChangeLayout (NextLayout),
                                                     IncMasterN (IncMasterN),
                                                     Resize (Expand, Shrink))
import           XMonad.Layout.BinarySpacePartition (ResizeDirectional (ExpandTowards, ShrinkFrom),
                                                     Rotate (Rotate),
                                                     Swap (Swap))
import           XMonad.Layout.Fullscreen           (FullscreenMessage (RemoveFullscreen))
import           XMonad.Layout.Gaps                 (GapMessage (ToggleGaps))
import           XMonad.Layout.MultiToggle          (Toggle (Toggle))
import           XMonad.Layout.ResizableTile        (MirrorResize (MirrorExpand, MirrorShrink))
import           XMonad.Operations                  (focus, mouseMoveWindow,
                                                     mouseResizeWindow, refresh,
                                                     restart, sendMessage,
                                                     setLayout, windows,
                                                     withFocused)
import           XMonad.Prompt.Shell                (shellPrompt)
import qualified XMonad.StackSet                    as W
import           XMonad.Util.Run                    (safeSpawn)
import           XMonad.Util.Types                  (Direction2D (D, L, R, U))

import           GridHelpers
import           GridSelectConfig
import           Layouts                            (ToggleFull (ToggleABitFull, ToggleVeryFull),
                                                     fullName)
import           Machines                           (Machine (..))
import           ProgramHelper
import           PromptConfig
import           Workspaces

sm :: KeyMask
sm = shiftMask

cm :: KeyMask
cm = controlMask

am :: KeyMask
am = mod1Mask

navigation2DConfig :: Navigation2DConfig
navigation2DConfig = def { defaultTiledNavigation = hybridOf (sideNavigationWithBias 0) centerNavigation
                         , layoutNavigation       = [(fullName, centerNavigation)]
                         , unmappedWindowRect     = [(fullName, fullScreenRect)]
                         }

tryMsg :: (Message a, Message b) => a -> b -> X ()
tryMsg x y = tryMessageB sendSomeMessageB x y >> pure ()

-- -- | Make a submap where each keybinding works with and without a modmask
-- makeSubmap :: KeyMask -> [(KeySym, X ())] -> X ()
-- makeSubmap mm sub = submap $ M.fromList $ concatMap keys sub
--   where keys (ks, x) = [ ((noModMask, ks), x)
--                        , ((mm       , ks), x)
--                        ]


keyBindings :: Machine -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
keyBindings mch conf@XConfig {XC.modMask = mm} = M.fromList $
    [
      -- quit
      ((mm .|. sm,        xK_q         ), liftIO exitSuccess)
      -- restart
    , ((mm,               xK_q         ), restart "xmonad" True)

      -- move focus up or down the window stack
    , ((mm,               xK_Tab       ), windows W.focusDown)
    , ((mm .|. sm,        xK_Tab       ), windows W.focusUp)

      -- modifying the window order
    , ((mm,               xK_Return    ), windows W.swapMaster)

      -- 2D navigation
    , ((mm,               xK_h         ), windowGo L False)
    , ((mm,               xK_j         ), windowGo D False)
    , ((mm,               xK_k         ), windowGo U False)
    , ((mm,               xK_l         ), windowGo R False)

      -- 2D swapping
    , ((mm .|. cm,        xK_h         ), windowSwap L False)
    , ((mm .|. cm,        xK_j         ), windowSwap D False)
    , ((mm .|. cm,        xK_k         ), windowSwap U False)
    , ((mm .|. cm,        xK_l         ), windowSwap R False)

      -- resizing the master/slave ratio
    , ((mm .|. sm,        xK_h         ), tryMsg (ExpandTowards L) Shrink)
    , ((mm .|. sm,        xK_j         ), tryMsg (ExpandTowards D) MirrorShrink)
    , ((mm .|. sm,        xK_k         ), tryMsg (ExpandTowards U) MirrorExpand)
    , ((mm .|. sm,        xK_l         ), tryMsg (ExpandTowards R) Expand)
    , ((mm .|. cm .|. sm, xK_h         ), tryMsg (ShrinkFrom R) Shrink)
    , ((mm .|. cm .|. sm, xK_j         ), tryMsg (ShrinkFrom D) MirrorExpand)
    , ((mm .|. cm .|. sm, xK_k         ), tryMsg (ShrinkFrom U) MirrorShrink)
    , ((mm .|. cm .|. sm, xK_l         ), tryMsg (ShrinkFrom L) Expand)

      -- increase or decrease number of windows in the master area
      -- or rotate and swap in BSP
    , ((mm,               xK_comma     ), tryMsg Rotate (IncMasterN 1))
    , ((mm,               xK_period    ), tryMsg Swap (IncMasterN (-1)))

      -- launch terminal
    , ((mm .|. sm,        xK_Return    ), runTerminal mch)
      -- launch prompt
    , ((mm,               xK_p         ), shellPrompt xpConfig)
    , ((mm,               xK_grave     ), quickTerm)
      -- kill the focused window
    , ((mm,               xK_BackSpace ), kill1)
      -- unfloat the current window
    , ((mm,               xK_t         ), withFocused $ windows . W.sink)

      -- rotate through available layouts
    , ((mm,               xK_space     ), sendMessage NextLayout)
      -- reset the layouts on the current workspace to default
    , ((mm .|. sm,        xK_space     ), setLayout (XC.layoutHook conf))
      -- resize viewed windows to the correct size
    , ((mm .|. sm,        xK_n         ), refresh)
      -- toggle fullscreen
    , ((mm,               xK_f         ), sendMessage $ Toggle ToggleABitFull)
    , ((mm .|. sm,        xK_f         ), sendMessage $ Toggle ToggleVeryFull)
    , ((mm .|. am,        xK_f         ), withFocused $ \w -> sendMessage $ RemoveFullscreen w)

      -- toggle narrowing
    , ((mm,               xK_n         ), sendMessage ToggleGaps)
      -- toggle struts
    , ((mm,               xK_b         ), sendMessage ToggleStruts)

      -- grid: go to window
    , ((mm,               xK_g         ), goToSelected $ gsConfig mm)
      -- grid: bring window
    , ((mm .|. sm,        xK_g         ), bringSelected $ gsConfig mm)
      -- grid: copy window to focus workspace (make it sticky)
    , ((mm,               xK_s         ), bringWindowCopy $ gsConfig mm)
      -- move window to minimsed workspace
    , ((mm,               xK_m         ), withFocused
                                          $ \w -> windows (W.shiftWin minimisedWS w) >>
                                                  windows (W.sink w))
    , ((mm .|. sm,        xK_m         ), bringWorkspaceWindow minimisedWS
                                          $ gsConfig mm)
      -- applications submap
    , ((mm,               xK_a         ), launchersMap mch mm)
    ]
    ++
    -- mod-[1..9] - switch to workspace N
    -- mod-shift-[1..9] - move client to workspace N
    [ ((mm .|. modifier, key), windows $ f workspace)
      | (workspace, key) <- zip (XC.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
      , (f, modifier) <- [ (W.greedyView, noModMask)
                         , (W.shift, sm)
                         , (copy, sm .|. cm)
                         ]
    ]
    ++ case mch of
         Cobalt -> [ -- volume controls
                     ((noModMask, xF86XK_AudioMute), safeSpawn "pactl" ["set-sink-mute", "@DEFAULT_SINK@", "toggle"])
                   , ((noModMask, xF86XK_AudioLowerVolume), safeSpawn "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "-10%"])
                   , ((noModMask, xF86XK_AudioRaiseVolume), safeSpawn "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "+10%"])
                     -- brightness controls
                   , ((mm .|. am, xK_minus), safeSpawn "brightnessctl" ["set", "5%-"])
                   , ((mm .|. am, xK_equal), safeSpawn "brightnessctl" ["set", "+5%"])
                   ]
         _      -> []

mouseBindings :: Machine -> XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings _ XConfig {XC.modMask = mm} = M.fromList
    -- set the window to floating mode and move by dragging
    [ ((mm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    -- raise the window to the top of the stack
    , ((mm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    -- set the window to floating mode and resize by dragging
    , ((mm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]
