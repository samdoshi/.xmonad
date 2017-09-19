module GridSelectConfig where

import           Data.Default              (def)
import qualified Data.Map                  as M
import           Graphics.X11.Types        (KeyMask, Window, noModMask,
                                            shiftMask, xK_Escape, xK_Return,
                                            xK_Tab, xK_h, xK_j, xK_k, xK_l,
                                            xK_n, xK_p, xK_slash)
import           XMonad.Actions.GridSelect (GSConfig, TwoD, cancel,
                                            makeXEventhandler, move, moveNext,
                                            movePrev, select, shadowWithKeymap,
                                            substringSearch)
import qualified XMonad.Actions.GridSelect as GS (GSConfig (..))
import           XMonad.Core               (X, runQuery)
import           XMonad.ManageHook         (className)

import           ProgramHelper
import           Theme

gsConfig :: KeyMask -> GSConfig Window
gsConfig mm = def { GS.gs_cellheight   = 80
                  , GS.gs_cellwidth    = 500
                  , GS.gs_cellpadding  = 10
                  , GS.gs_colorizer    = windowClassColouriser
                  , GS.gs_font         = font 24
                  , GS.gs_navigate     = navigation mm
                  , GS.gs_originFractX = 1/2
                  , GS.gs_originFractY = 1/3
                  , GS.gs_bordercolor  = base1
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

windowClassColouriser :: Window -> Bool -> X (String, String)
windowClassColouriser win act = do
  cn <- runQuery className win
  let colour = colourForClass cn
  if act
    then return (colour, base2)
    else return (base02, colour)

colourForClass :: String -> String
colourForClass c | isBrowser c  = blue
                 | isEmacs c    = violet
                 | isTerminal c = yellow
                 | otherwise    = base1
