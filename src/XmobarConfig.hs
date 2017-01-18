module XmobarConfig ( xmobar
                    ) where

import           System.IO               (Handle)

import           Data.Default            (def)
import           XMonad.Core             (WorkspaceId, XConfig)
import qualified XMonad.Core             as XC (XConfig (..))
import           XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP,
                                          xmobarColor)
import           XMonad.Util.Run         (hPutStrLn, spawnPipe)

import           Solarized
import           Workspaces

xmobar :: XConfig a -> IO (XConfig a)
xmobar c = do
    h <- spawnPipe "xmobar /home/sam/.xmonad/xmobar/xmobarrc"
    pure $ c { XC.logHook = XC.logHook c >> dynamicLogWithPP (pp h) }

pp :: Handle -> PP
pp h = def { ppCurrent         = xmobarColor blue "" . fnBold . awesome
           , ppHidden          = xmobarColor base1 "" . awesome
           , ppHiddenNoWindows = xmobarColor base03 "" . awesome . hideEmpty
           , ppTitle           = xmobarColor blue "" . fnBold . shorten 128
           , ppVisible         = wrap "(" ")" -- Xinerama only
           , ppUrgent          = xmobarColor red yellow
           , ppSep             = "  "
           , ppWsSep           = " "
           , ppOutput          = hPutStrLn h
           }

awesome :: WorkspaceId -> String
awesome x | x == homeWS      = fnAwesome "\xf015"
          | x == alphaWS     = "\x03b1"
          | x == betaWS      = "\x03b2"
          | x == mediaWS     = fnAwesome "\xf03e"
          | x == gamesWS     = fnAwesome "\xf11b"
          | x == floatWS     = fnAwesome "\xf2d2"
          | x == minimisedWS = fnAwesome "\xf00a"
          | otherwise = x

hideEmpty :: String -> String
hideEmpty x | x `elem` ["7", "8", "9"] = ""
            | otherwise                = x

fnBold :: String -> String
fnBold = wrap "<fn=1>" "</fn>"

fnAwesome :: String -> String
fnAwesome = wrap "<fn=2>" "</fn>"

wrap :: String -> String -> String -> String
wrap _ _ "" = ""
wrap l r m  = l ++ m ++ r

shorten :: Int -> String -> String
shorten n xs | length xs < n = xs
             | otherwise     = take (n - length end) xs ++ end

end :: String
end = "..."
