module PolybarConfig ( polybar
                     ) where

import           Data.Default             (def)

import           Codec.Binary.UTF8.String (decodeString)
import qualified DBus                     as D
import qualified DBus.Client              as D
import           XMonad.Core              (WorkspaceId, XConfig)
import qualified XMonad.Core              as XC (XConfig (..))
import           XMonad.Hooks.DynamicLog  (PP (..), dynamicLogWithPP)
import           XMonad.Util.Run          (unsafeSpawn)

import           Theme
import           Workspaces

polybar :: XConfig a -> IO (XConfig a)
polybar xc = do
  unsafeSpawn "killall -qu polybar; polybar --config=/home/sam/.xmonad/polybar/config main"
  dbus <- D.connectSession
  pure xc { XC.logHook = XC.logHook xc >> dynamicLogWithPP (pp dbus) }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/com/samdoshi/xmonad/log"
    interfaceName = D.interfaceName_ "com.samdoshi.xmonad"
    memberName = D.memberName_ "Update"

pp :: D.Client -> PP
pp dbus = def { ppCurrent         = foreground orange . fnBold . material
              , ppHidden          = foreground base1 . material
              , ppHiddenNoWindows = foreground base03 . material . hideEmpty
              , ppTitle           = foreground blue . fnBold . shorten 128
              , ppVisible         = wrap "(" ")" -- Xinerama only
              , ppUrgent          = foreground urgent . fnBold . material
              , ppSep             = "  "
              , ppWsSep           = " "
              , ppOutput          = dbusOutput dbus
              }

material :: WorkspaceId -> String
material x | x == homeWS      = "\xe88a"
           | x == alphaWS     = "\x03b1"
           | x == betaWS      = "\x03b2"
           | x == mediaWS     = "\xe54d"
           | x == gamesWS     = "\xe338"
           | x == floatWS     = "\xe8aa"
           | x == minimisedWS = "\xe5c3"
           | otherwise = x

hideEmpty :: String -> String
hideEmpty x | x `elem` ["7", "8", "9"] = ""
            | otherwise                = x

fnBold :: String -> String
fnBold = polybarFormat "T" "2"

foreground :: String -> String -> String
foreground = polybarFormat "F"

polybarFormat :: String -> String -> String -> String
polybarFormat _   _     ""     = ""
polybarFormat tag value string = "%{" ++ tag ++ value ++ "}" ++ string ++ "%{" ++ tag ++ "-}"


wrap :: String -> String -> String -> String
wrap _ _ "" = ""
wrap l r m  = l ++ m ++ r

shorten :: Int -> String -> String
shorten n xs | length xs < n = xs
             | otherwise     = take (n - length end) xs ++ end

end :: String
end = "..."
