{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Layouts ( layoutHook
               , tiledName
               , bspName
               , bigName
               , fullName
               , floatName
               , ToggleFull(ToggleFull)
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
import           XMonad.Layout.Grid                 (Grid (Grid))
import           XMonad.Layout.LayoutModifier       (ModifiedLayout)
import           XMonad.Layout.MultiToggle          (EOT, HCons, MultiToggle,
                                                     Transformer (transform),
                                                     mkToggle, single)
import           XMonad.Layout.NoBorders            (SmartBorder, WithBorder,
                                                     noBorders, smartBorders)
import           XMonad.Layout.NoFrillsDecoration   (NoFrillsDecoration,
                                                     noFrillsDeco)
import           XMonad.Layout.PerWorkspace         (PerWorkspace, onWorkspace)
import           XMonad.Layout.Renamed              (Rename (Replace), renamed)
import           XMonad.Layout.ResizableTile        (ResizableTall (ResizableTall))
import           XMonad.Layout.Simplest             (Simplest (Simplest))
import           XMonad.Layout.SimplestFloat        (SimplestFloat,
                                                     simplestFloat)
import           XMonad.Layout.Spacing              (Spacing, spacing)
import           XMonad.Layout.SubLayouts           (Sublayout, subLayout)
import           XMonad.Layout.Tabbed               (TabbedDecoration, addTabs)
import           XMonad.Layout.WindowArranger       (WindowArranger)
import           XMonad.Layout.WindowNavigation     (WindowNavigation,
                                                     windowNavigation)

import           OneBig                             (OneBig (OneBig))
import           Solarized
import           Workspaces

-- Shorten some common types
type ML = ModifiedLayout
type CH = Choose
type PW = PerWorkspace

-- The main layout hook, uses `PerWorkspace`
type LayoutHook = PW FloatChoice
                  (PW MediaChoice
                   (PW MinimisedChoice DefaultChoice))
layoutHook :: LayoutHook Window
layoutHook = onWorkspace floatWS floatChoice
             $ onWorkspace mediaWS mediaChoice
             $ onWorkspace minimisedWS minimisedChoice
             defaultChoice

-- Predefined layout choices
type DefaultChoice = CH TiledLayout BSPLayout
defaultChoice :: DefaultChoice Window
defaultChoice = tiled ||| bsp

type FloatChoice = CH FloatLayout FullLayout
floatChoice :: FloatChoice Window
floatChoice = float ||| full

type MediaChoice = CH BigLayout FullLayout
mediaChoice :: MediaChoice Window
mediaChoice = big ||| full

type MinimisedChoice = GridLayout
minimisedChoice :: MinimisedChoice Window
minimisedChoice = grid

-- Standard tiled layout
tiledName :: String
tiledName = "tiled"

type TiledLayout = EmbellishedLayout ResizableTall
tiled :: TiledLayout Window
tiled = embellish tiledName $ ResizableTall 1 (2/100) (1/2) []

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

-- Full screen layout
fullName :: String
fullName = "full"

type FullLayout = ML Rename (ML SmartBorder Full)
full :: FullLayout a
full = rename fullName $ smartBorders Full

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

data ToggleFull = ToggleFull
                deriving (Show, Read, Eq, Typeable)

instance Transformer ToggleFull Window where
  transform ToggleFull x k = k full (const x)

toggleFull :: LayoutClass l Window
           => l Window
           -> MultiToggle (HCons ToggleFull EOT) l Window
toggleFull = mkToggle (single ToggleFull)

type EmbellishedLayout a = MultiToggle (HCons ToggleFull EOT)
                           (ML Rename
                            (ML AvoidStruts
                             (ML WindowNavigation
                              (ML WithBorder
                               (ML TopBarDecoration
                                (ML (Decoration TabbedDecoration DefaultShrinker)
                                 (ML (Sublayout Simplest)
                                  (ML Spacing
                                   a))))))))
embellish :: LayoutClass l Window
          => String
          -> l Window
          -> EmbellishedLayout l Window
embellish s l = toggleFull
              $ rename s
              $ avoidStruts
              $ windowNavigation -- needed for subLayouts
              $ noBorders
              $ topBarDecoration
              $ addTabs shrinkText floatTheme
              $ subLayout [] Simplest
              $ spacing 6
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
                 , T.fontName = "xft:Roboto Mono:pixelsize=16"
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
                  , T.fontName = "xft:Roboto Mono:pixelsize=5"
                  , T.decoHeight = 12
                  }
