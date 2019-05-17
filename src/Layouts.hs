{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Layouts ( layoutHook
               , tiledName
               , bspName
               , bigName
               , fullName
               , floatName
               , ToggleFull(ToggleVeryFull, ToggleABitFull)
               ) where

import           Data.Default                       (def)
import           Data.Typeable                      (Typeable)
import           Graphics.X11.Types                 (Window)
import           XMonad.Actions.MouseResize         (MouseResize, mouseResize)
import           XMonad.Core                        (LayoutClass)
import           XMonad.Hooks.ManageDocks           (AvoidStruts, avoidStruts)
import           XMonad.Layout                      (Choose, Full (Full), (|||))
import           XMonad.Layout.BinarySpacePartition (BinarySpacePartition,
                                                     emptyBSP)
import           XMonad.Layout.BorderResize         (BorderResize, borderResize)
import           XMonad.Layout.Decoration           (Decoration,
                                                     DefaultShrinker, Theme,
                                                     shrinkText)
import qualified XMonad.Layout.Decoration           as T (Theme (..))
import           XMonad.Layout.Fullscreen           (FullscreenFocus,
                                                     fullscreenFocus)
import           XMonad.Layout.Gaps                 (Gaps, gaps')
import           XMonad.Layout.Grid                 (Grid (Grid))
import           XMonad.Layout.LayoutModifier       (ModifiedLayout)
import           XMonad.Layout.MultiToggle          (EOT (EOT), HCons,
                                                     MultiToggle,
                                                     Transformer (transform),
                                                     mkToggle, (??))
import           XMonad.Layout.NoBorders            (WithBorder, noBorders)
import           XMonad.Layout.NoFrillsDecoration   (NoFrillsDecoration,
                                                     noFrillsDeco)
import           XMonad.Layout.PerWorkspace         (PerWorkspace, onWorkspace)
import           XMonad.Layout.Renamed              (Rename (Replace), renamed)
import           XMonad.Layout.ResizableTile        (ResizableTall (ResizableTall))
import           XMonad.Layout.Simplest             (Simplest (Simplest))
import           XMonad.Layout.SimplestFloat        (SimplestFloat,
                                                     simplestFloat)
import           XMonad.Layout.Spacing              (Border (..), Spacing,
                                                     spacingRaw)
import           XMonad.Layout.SubLayouts           (Sublayout, subLayout)
import           XMonad.Layout.Tabbed               (TabbedDecoration, addTabs)
import           XMonad.Layout.WindowArranger       (WindowArranger)
import           XMonad.Layout.WindowNavigation     (WindowNavigation,
                                                     windowNavigation)
import           XMonad.Util.Types                  (Direction2D (L, R))

import           Flip
import           Machines                           (Machine)
import           OneBig                             (OneBig (OneBig))
import           Theme
import           Workspaces

-- Constants

-- How much gap between windows
windowGaps :: Border
windowGaps = uniformBorder 6

-- How wide should windows be in narrow mode
narrowWidth :: Int
narrowWidth = 1280

-- Helpers
uniformBorder :: Integer -> Border
uniformBorder i = Border i i i i

-- Shorten some common types
type ML = ModifiedLayout
type CH = Choose
type PW = PerWorkspace

-- The main layout hook, uses `PerWorkspace`
type LayoutHook = PW HomeChoice
                  (PW FloatChoice
                   (PW MediaChoice
                    (PW MinimisedChoice DefaultChoice)))
layoutHook :: Machine -> LayoutHook Window
layoutHook _ = onWorkspace homeWS homeChoice
               $ onWorkspace floatWS floatChoice
               $ onWorkspace mediaWS mediaChoice
               $ onWorkspace minimisedWS minimisedChoice
               defaultChoice

-- Predefined layout choices
type DefaultChoice = CH TiledLayout BSPLayout
defaultChoice :: DefaultChoice Window
defaultChoice = tiled ||| bsp

type HomeChoice = CH HomeTiledLayout BSPLayout
homeChoice :: HomeChoice Window
homeChoice = homeTiled ||| bsp

type FloatChoice = FloatLayout
floatChoice :: FloatChoice Window
floatChoice = float

type MediaChoice = BigLayout
mediaChoice :: MediaChoice Window
mediaChoice = big

type MinimisedChoice = GridLayout
minimisedChoice :: MinimisedChoice Window
minimisedChoice = grid

-- Standard tiled layout
tiledName :: String
tiledName = "tiled"

type TiledLayout = EmbellishedLayout ResizableTall
tiled :: TiledLayout Window
tiled = embellish tiledName $ ResizableTall 1 (2/100) (1/2) []

-- Home tiled layout
-- (speical tiled layout for the home workspace)
homeTiledName :: String
homeTiledName = "home"

type HomeTiledLayout = EmbellishedLayout (ML Flip ResizableTall)
homeTiled :: HomeTiledLayout Window
homeTiled = embellish homeTiledName $ flipLayout $ ResizableTall 1 (2/100) (2/3) []

-- BSP layout
bspName :: String
bspName = "bsp"

type BSPLayout = ML BorderResize (EmbellishedLayout BinarySpacePartition)
bsp :: BSPLayout Window
bsp = borderResize $ embellish bspName emptyBSP

-- Widescreen `OneBig` layout
bigName :: String
bigName = "big"

type BigLayout = EmbellishedLayout OneBig
big :: BigLayout Window
big = embellish bigName
      $ OneBig (3/4) (3/4)

-- Full screen layouts
fullName :: String
fullName = "full"

type VeryFullLayout = ML Rename (ML WithBorder Full)
full :: VeryFullLayout Window
full = rename fullName $ noBorders Full

type ABitFullLayout = ML Rename
                       (ML AvoidStruts
                        (ML Gaps
                         (ML WithBorder
                          (ML TopBarDecoration
                           (ML Spacing
                            Full)))))
aBitFull :: ABitFullLayout Window
aBitFull = rename fullName
           $ avoidStruts
           $ gaps' [ ((L, narrowWidth `quot` 2), False)
                   , ((R, narrowWidth `quot` 2), False)
                   ]
           $ noBorders
           $ topBarDecoration
           $ spacingRaw False (uniformBorder 0) False windowGaps True
           Full

-- Grid layout
gridName :: String
gridName = "grid"

type GridLayout = EmbellishedLayout Grid
grid :: GridLayout Window
grid = embellish gridName Grid

-- Floating layout
floatName :: String
floatName = "float"

type FloatLayout = ML Rename
                   (ML AvoidStruts
                    (ML FloatDecoration
                     (ML MouseResize
                      (ML WindowArranger SimplestFloat))))
float :: FloatLayout Window
float = rename floatName
        $ avoidStruts
        $ floatDecoration
        $ mouseResize
        simplestFloat

-- Helpers

rename :: String -> l a -> ML Rename l a
rename s = renamed [Replace s]

-- Toggles

data ToggleFull = ToggleVeryFull
                | ToggleABitFull
                deriving (Show, Read, Eq, Typeable)

instance Transformer ToggleFull Window where
  transform ToggleVeryFull x k = k full (const x)
  transform ToggleABitFull x k = k aBitFull (const x)

type ToggleFullMultiToggle a = MultiToggle (HCons ToggleFull (HCons ToggleFull EOT)) a
toggleFull :: LayoutClass l Window
           => l Window
           -> ToggleFullMultiToggle l Window
toggleFull = mkToggle (ToggleVeryFull ?? ToggleABitFull ?? EOT)

-- Main embellishments

type EmbellishedLayout a = ToggleFullMultiToggle
                           (ML Rename
                            (ML FullscreenFocus
                             (ML AvoidStruts
                              (ML Gaps
                               (ML WindowNavigation
                                (ML WithBorder
                                 (ML TopBarDecoration
                                  (ML (Decoration TabbedDecoration DefaultShrinker)
                                   (ML (Sublayout Simplest)
                                    (ML Spacing
                                     a))))))))))
embellish :: LayoutClass l Window
          => String
          -> l Window
          -> EmbellishedLayout l Window
embellish s l = toggleFull                     -- use a message to toggle fullscreen
              $ rename s                       -- renamed layout
              $ fullscreenFocus                -- only allow EWMH fullscreen when focused
              $ avoidStruts                    -- avoid docks, bars, etc
              $ gaps'                          -- width for narrow mode, initially off
                [ ((L, narrowWidth `quot` 2), False)
                , ((R, narrowWidth `quot` 2), False)
                ]
              $ windowNavigation               -- needed for subLayouts
              $ noBorders                      -- disable borders, we'll use a theme
              $ topBarDecoration               -- add top bar theme
              $ addTabs shrinkText floatTheme  -- tabs support
              $ subLayout [] Simplest          -- tabs support
              $ spacingRaw False (uniformBorder 0) False windowGaps True -- spacing
              l

-- Themes

type FloatDecoration = Decoration NoFrillsDecoration DefaultShrinker
floatDecoration :: Eq a => l a -> ML FloatDecoration l a
floatDecoration = noFrillsDeco shrinkText floatTheme

floatTheme :: Theme
floatTheme = def { T.activeColor = active
                 , T.inactiveColor = inactive
                 , T.urgentColor = urgent
                 , T.activeBorderColor = active
                 , T.inactiveBorderColor = inactive
                 , T.urgentBorderColor = urgent
                 , T.activeTextColor = base2
                 , T.inactiveTextColor = base1
                 , T.urgentTextColor = base2
                 , T.fontName = font 16
                 , T.decoHeight = 24
                 }

type TopBarDecoration = Decoration NoFrillsDecoration DefaultShrinker
topBarDecoration :: Eq a => l a -> ML TopBarDecoration l a
topBarDecoration = noFrillsDeco shrinkText topBarTheme

topBarTheme :: Theme
topBarTheme = def { T.activeColor = active
                  , T.inactiveColor = inactive
                  , T.urgentColor = urgent
                  , T.activeBorderColor = active
                  , T.inactiveBorderColor = inactive
                  , T.urgentBorderColor = urgent
                  , T.activeTextColor = active
                  , T.inactiveTextColor = inactive
                  , T.urgentTextColor = red
                  , T.fontName = font 5
                  , T.decoHeight = 12
                  }
