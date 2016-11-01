module Layouts ( layoutHook
               ) where

import           Data.Default                     (def)
import           Graphics.X11.Types               (Window)
import           XMonad.Actions.MouseResize       (MouseResize, mouseResize)
import           XMonad.Hooks.ManageDocks         (AvoidStruts, avoidStruts)
import           XMonad.Layout                    (Choose, Full (Full),
                                                   Tall (Tall), (|||))
import           XMonad.Layout.Decoration         (Decoration, DefaultShrinker,
                                                   Theme, shrinkText)
import qualified XMonad.Layout.Decoration         as T (Theme (..))
import           XMonad.Layout.LayoutModifier     (ModifiedLayout)
import           XMonad.Layout.NoBorders          (SmartBorder, smartBorders)
import           XMonad.Layout.NoFrillsDecoration (NoFrillsDecoration,
                                                   noFrillsDeco)
import           XMonad.Layout.PerWorkspace       (PerWorkspace, onWorkspace)
import           XMonad.Layout.Renamed            (Rename (Replace), renamed)
import           XMonad.Layout.SimplestFloat      (SimplestFloat, simplestFloat)
import           XMonad.Layout.Spacing            (Spacing, spacing)
import           XMonad.Layout.WindowArranger     (WindowArranger)

import           Solarized
import           Workspaces

-- Shorten some common types
type ML = ModifiedLayout
type CH = Choose
type PW = PerWorkspace

type LayoutHook = PW FloatChoice DefaultChoice
layoutHook :: LayoutHook Window
layoutHook = onWorkspace floatWS floatChoice
             defaultChoice

type DefaultChoice = CH TiledLayout FullLayout
defaultChoice :: DefaultChoice Window
defaultChoice = tiled ||| full

type FloatChoice = CH FloatLayout FullLayout
floatChoice :: FloatChoice Window
floatChoice = float ||| full

type FloatLayout = ML Rename
                   (ML CustomDecoration
                    (ML MouseResize
                     (ML WindowArranger SimplestFloat)))
float :: Eq a => FloatLayout a
float = renamed [Replace "float"]
        $ customDecoration
        $ mouseResize
        simplestFloat

type TiledLayout = ML Rename (ML AvoidStruts (ML Spacing Tall))
tiled :: TiledLayout a
tiled = renamed [Replace "tiled"]
        $ avoidStruts
        $ spacing 3
        $ Tall 1 (2/100) (1/2)

type FullLayout = ML Rename (ML SmartBorder Full)
full :: FullLayout a
full = renamed [Replace "full"] $ smartBorders Full

type CustomDecoration = Decoration NoFrillsDecoration DefaultShrinker
customDecoration :: Eq a => l a -> ML CustomDecoration l a
customDecoration = noFrillsDeco shrinkText theme

theme :: Theme
theme = def { T.activeColor = orange
            , T.inactiveColor = base01
            , T.urgentColor = red
            , T.activeBorderColor = orange
            , T.inactiveBorderColor = base01
            , T.urgentBorderColor = red
            , T.activeTextColor = base2
            , T.inactiveTextColor = base1
            , T.urgentTextColor = base2
            , T.fontName = "xft:Roboto Mono:pixelsize=14"
            , T.decoHeight = 24
            }
