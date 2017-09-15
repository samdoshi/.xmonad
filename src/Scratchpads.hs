module Scratchpads where

import           XMonad.ManageHook
import           XMonad.StackSet             (RationalRect (..))
import           XMonad.Util.NamedScratchpad

goldenDictScratchpad :: String
goldenDictScratchpad = "goldenDict"

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS { name = goldenDictScratchpad
       , cmd = "goldendict"
       , query = className =? "GoldenDict"
       , hook = customFloating $ RationalRect (1/6) (1/6) (2/3) (2/3)
       }
  ]
