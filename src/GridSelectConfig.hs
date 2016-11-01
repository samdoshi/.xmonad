module GridSelectConfig where

import           Data.Default              (def)

import           XMonad.Actions.GridSelect (GSConfig (..), HasColorizer)

gsConfig :: HasColorizer a => GSConfig a
gsConfig = def { gs_cellheight = 40
               , gs_cellwidth = 400
               , gs_cellpadding = 5
               , gs_font = "xft:Roboto Mono:pixelsize=16"
               , gs_originFractX = 1/2
               , gs_originFractY = 1/3
               }
