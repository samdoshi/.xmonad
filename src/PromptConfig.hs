module PromptConfig where

import           Data.Default             (def)

import           XMonad.Prompt            (XPConfig, XPPosition (CenteredAt))
import qualified XMonad.Prompt            as XP (XPConfig (..))
import           XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)

import           Theme

xpConfig :: XPConfig
xpConfig = def { XP.font              = font 30
               , XP.bgColor           = base02
               , XP.fgColor           = base1
               , XP.fgHLight          = active
               , XP.bgHLight          = base02
               , XP.borderColor       = base01
               , XP.promptBorderWidth = 10
               , XP.position          = CenteredAt 0.125 0.5
               , XP.height            = 100
               }

xpFuzzyConfig :: XPConfig
xpFuzzyConfig = xpConfig { XP.searchPredicate = fuzzyMatch
                         , XP.sorter = fuzzySort
                         }
