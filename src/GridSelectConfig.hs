module GridSelectConfig where

import           Data.Default              (def)
import qualified Data.Map                  as M
import           Graphics.X11.Types
import           XMonad.Actions.GridSelect
import           XMonad.Core               (X)

import           Solarized

gsConfig :: HasColorizer a => KeyMask -> GSConfig a
gsConfig mm = def { gs_cellheight = 40
                  , gs_cellwidth = 400
                  , gs_cellpadding = 5
                  , gs_colorizer = boringColouriser
                  , gs_font = "xft:Roboto Mono:pixelsize=16"
                  , gs_navigate = navigation mm
                  , gs_originFractX = 1/2
                  , gs_originFractY = 1/3
                  }

navigation :: KeyMask -> TwoD a (Maybe a)
navigation mm = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where navigation' = navigation mm
        navKeyMap = M.fromList
          [ ((noModMask, xK_Escape), cancel)
          , ((noModMask, xK_Return), select)
          , ((mm,        xK_Return), select)
          , ((noModMask, xK_slash) , substringSearch navigation')
          , ((noModMask, xK_h)     , move (-1,  0) >> navigation')
          , ((mm,        xK_h)     , move (-1,  0) >> navigation')
          , ((noModMask, xK_l)     , move ( 1,  0) >> navigation')
          , ((mm,        xK_l)     , move ( 1,  0) >> navigation')
          , ((noModMask, xK_j)     , move ( 0,  1) >> navigation')
          , ((mm,        xK_j)     , move ( 0,  1) >> navigation')
          , ((noModMask, xK_k)     , move ( 0, -1) >> navigation')
          , ((mm,        xK_k)     , move ( 0, -1) >> navigation')
          , ((noModMask, xK_Tab)   , moveNext >> navigation')
          , ((noModMask, xK_n)     , moveNext >> navigation')
          , ((shiftMask, xK_Tab)   , movePrev >> navigation')
          , ((noModMask, xK_p)     , movePrev >> navigation')
          ]
        navDefaultHandler = const navigation'

boringColouriser :: a -> Bool -> X (String, String)
boringColouriser _ True  = pure (base02, active)
boringColouriser _ False = pure (base02, base1)
