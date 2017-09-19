module Keys ( navigation2DConfig
            , keys
            , mouseBindings
            ) where

import           Control.Monad.IO.Class             (liftIO)
import           Data.Bits                          ((.|.))
import           Data.Default                       (def)
import           Data.Map                           (Map)
import qualified Data.Map                           as M
import           System.Exit                        (exitSuccess)

import           Graphics.X11.Types                 (Button, KeyMask, KeySym,
                                                     Window, button1, button2,
                                                     button3, controlMask,
                                                     mod1Mask, shiftMask, xK_0,
                                                     xK_1, xK_9, xK_BackSpace,
                                                     xK_Return, xK_Tab, xK_a,
                                                     xK_apostrophe, xK_b,
                                                     xK_backslash, xK_comma,
                                                     xK_d, xK_e, xK_f, xK_g,
                                                     xK_h, xK_i, xK_j, xK_k,
                                                     xK_l, xK_m, xK_minus, xK_n,
                                                     xK_o, xK_p, xK_period,
                                                     xK_q, xK_r, xK_s,
                                                     xK_semicolon, xK_space,
                                                     xK_t, xK_u, xK_w)
import           XMonad.Actions.CopyWindow          (kill1)
import           XMonad.Actions.GridSelect          (bringSelected,
                                                     goToSelected)
import           XMonad.Actions.MessageFeedback     (tryMessage_)
import           XMonad.Actions.Navigation2D        (Navigation2DConfig,
                                                     centerNavigation,
                                                     defaultTiledNavigation,
                                                     fullScreenRect,
                                                     hybridNavigation,
                                                     layoutNavigation,
                                                     unmappedWindowRect,
                                                     windowGo, windowSwap)
import           XMonad.Actions.Submap              (submap)
import           XMonad.Actions.WindowGo            (raiseNextMaybe)
import           XMonad.Core                        (Layout, Message, X,
                                                     XConfig (XConfig),
                                                     whenJust)
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
import           XMonad.Layout.SubLayouts           (GroupMsg (UnMerge),
                                                     onGroup, pullGroup, toSubl)
import           XMonad.Operations                  (focus, mouseMoveWindow,
                                                     mouseResizeWindow, refresh,
                                                     restart, screenWorkspace,
                                                     sendMessage, setLayout,
                                                     windows, withFocused)
import           XMonad.Prompt.Shell                (shellPrompt)
import qualified XMonad.StackSet                    as W
import           XMonad.Util.NamedScratchpad        (namedScratchpadAction)
import           XMonad.Util.Run                    (safeSpawnProg)
import           XMonad.Util.Types                  (Direction2D (D, L, R, U))


import           GridHelpers
import           GridSelectConfig
import           Layouts                            (ToggleFull (ToggleABitFull, ToggleVeryFull),
                                                     fullName)
import           PassPrompt
import           ProgramHelper
import           PromptConfig
import           Scratchpads
import           Workspaces

sm :: KeyMask
sm = shiftMask

cm :: KeyMask
cm = controlMask

am :: KeyMask
am = mod1Mask

navigation2DConfig :: Navigation2DConfig
navigation2DConfig = def { defaultTiledNavigation = hybridNavigation
                         , layoutNavigation       = [(fullName, centerNavigation)]
                         , unmappedWindowRect     = [(fullName, fullScreenRect)]
                         }

tryMsg :: (Message a, Message b) => a -> b -> X ()
tryMsg x y = tryMessage_ x y >> refresh

keys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
keys conf@XConfig {XC.modMask = mm} = M.fromList $
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
    , ((mm,               xK_h         ), windowGo L True)
    , ((mm,               xK_j         ), windowGo D True)
    , ((mm,               xK_k         ), windowGo U True)
    , ((mm,               xK_l         ), windowGo R True)

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

      -- tabs
    , ((mm .|. am,        xK_h         ), sendMessage $ pullGroup L)
    , ((mm .|. am,        xK_j         ), sendMessage $ pullGroup D)
    , ((mm .|. am,        xK_k         ), sendMessage $ pullGroup U)
    , ((mm .|. am,        xK_l         ), sendMessage $ pullGroup R)
    , ((mm,               xK_semicolon ), onGroup W.focusDown')
    , ((mm,               xK_apostrophe), onGroup W.focusUp')
    , ((mm .|. sm,        xK_semicolon ), withFocused (sendMessage . UnMerge))
    , ((mm .|. sm,        xK_apostrophe), toSubl NextLayout)

      -- launch terminal
    , ((mm .|. sm,        xK_Return    ), runTerminal)
      -- launch prompt
    , ((mm,               xK_p         ), shellPrompt xpConfig)
      -- 1passkell
    , ((mm,               xK_backslash ), sendUsernamePasswordPrompt xpConfig)
      -- raise next or run
    , ((mm,               xK_u         ), raiseNextMaybe runTerminal isTerminal')
    , ((mm .|. sm,        xK_u         ), runTerminal)
    , ((mm,               xK_i         ), raiseNextMaybe runBrowser isBrowser')
    , ((mm .|. sm,        xK_i         ), runBrowser)
    , ((mm,               xK_o         ), raiseNextMaybe runEmacs isEmacs')
    , ((mm .|. sm,        xK_o         ), runEmacs)

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

      -- go to window
    , ((mm,               xK_g         ), goToSelected $ gsConfig mm)
      -- bring window
    , ((mm .|. sm,        xK_g         ), bringSelected $ gsConfig mm)
      -- copy window to focus workspace (make it sticky)
    , ((mm,               xK_s         ), bringWindowCopy $ gsConfig mm)
      -- move window to minimsed workspace
    , ((mm,               xK_m         ), withFocused
                                          $ windows . W.shiftWin minimisedWS)
    , ((mm .|. sm,        xK_m         ), bringWorkspaceWindow minimisedWS
                                          $ gsConfig mm)

      -- named scratchpads submap
    , ((mm,               xK_a         ), submap . M.fromList $
        [ ((mm, xK_d), namedScratchpadAction scratchpads goldenDictScratchpad)
        ])
    ]
    ++
    -- mod-[1..9] - switch to workspace N
    -- mod-shift-[1..9] - move client to workspace N
    [((mm .|. m, k), windows $ f i)
        | (i, k) <- zip (XC.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0, xK_minus])
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
