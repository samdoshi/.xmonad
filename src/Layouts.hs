module Layouts ( layoutHook
               ) where

import           Graphics.X11.Types           (Window)
import           XMonad.Hooks.ManageDocks     (AvoidStruts, avoidStruts)
import           XMonad.Layout                (Choose, Full (Full), Tall (Tall),
                                               (|||))
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Layout.NoBorders      (SmartBorder, smartBorders)
import           XMonad.Layout.Renamed        (Rename (Replace), renamed)
import           XMonad.Layout.Spacing        (Spacing, spacing)

type ML = ModifiedLayout
type TallLayout = ML Rename (ML AvoidStruts (ML Spacing Tall))
type FullLayout = ML SmartBorder Full
type ChooseLayout = Choose TallLayout FullLayout
type LayoutHook = ChooseLayout

spacingWidth :: Int
spacingWidth = 3

layoutHook :: LayoutHook Window
layoutHook = tall' ||| smartBorders Full
  where tall = avoidStruts $ spacing spacingWidth $ Tall 1 (2/100) (1/2)
        tall' = renamed [Replace "Tall"] tall
