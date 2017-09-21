module Scratchpads where

import           XMonad.ManageHook
import           XMonad.StackSet             (RationalRect (..))
import           XMonad.Util.NamedScratchpad

goldenDictScratchpad :: String
goldenDictScratchpad = "goldenDict"

qalculateScrachpad :: String
qalculateScrachpad = "qalculate"

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS { name = goldenDictScratchpad
       , cmd = "goldendict"
       , query = className =? "GoldenDict"
       , hook = customFloating $ RationalRect (1/6) (1/6) (2/3) (2/3)
       }
  , NS { name = qalculateScrachpad
       , cmd = "qalculate-gtk"
       , query = className =? "Qalculate-gtk"
       , hook = customFloating $ RationalRect (1/6) (1/6) (2/3) (2/3)
       }
  ]
