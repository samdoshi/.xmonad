{-# LANGUAGE OverloadedStrings #-}

module PolybarConfig ( polybar
                     ) where

import           Data.List                    (intercalate)
import           Data.Maybe                   (isJust, listToMaybe)

import           Control.Monad.State.Class    (gets)
import           Control.Monad.Trans          (liftIO)
import qualified DBus                         as D
import qualified DBus.Client                  as D
import           Graphics.X11.Types           (Window)
import           System.Directory             (getHomeDirectory)
import           System.FilePath              ((</>))
import           XMonad.Core                  (Layout, ScreenId, WindowSet,
                                               WorkspaceId, X, XConfig,
                                               XState (windowset), description)
import qualified XMonad.Core                  as XC (XConfig (..))
import           XMonad.Hooks.UrgencyHook     (readUrgents)
import qualified XMonad.StackSet              as S
import           XMonad.Util.NamedWindows     (getName)
import           XMonad.Util.Run              (unsafeSpawn)
import           XMonad.Util.WorkspaceCompare (WorkspaceSort, getSortByIndex)

import           Machines
import           Theme
import           Workspaces


data Bar = BarLeft | BarRight | BarOne

polybar :: Machine -> XConfig a -> IO (XConfig a)
polybar mch xc = do
  homeDir <- getHomeDirectory
  unsafeSpawn $ homeDir </> ".xmonad/polybar/launch.sh"
  dbus <- D.connectSession
  pure xc { XC.logHook = XC.logHook xc
                         >> polybarHook dbus mch BarLeft
                         >> polybarHook dbus mch BarRight
                         >> polybarHook dbus mch BarOne
          }

screenForBar :: Bar -> ScreenId
screenForBar BarLeft  = 0
screenForBar BarRight = 1
screenForBar BarOne   = 0

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> Bar -> String -> X ()
dbusOutput dbus bar str = do
    let signal = (D.signal (objectPath bar) interfaceName memberName) {
            D.signalBody = [D.toVariant str]
        }
    liftIO $ D.emit dbus signal
  where
    objectPath BarLeft  = "/com/samdoshi/xmonad/left"
    objectPath BarRight = "/com/samdoshi/xmonad/right"
    objectPath BarOne   = "/com/samdoshi/xmonad/one"
    interfaceName = "com.samdoshi.xmonad"
    memberName = "Update"

polybarHook :: D.Client -> Machine -> Bar -> X ()
polybarHook dbus _ bar =
  do ws <- gets windowset
     urgents <- readUrgents
     title <- maybe (return "") (fmap show . getName) . S.peek $ ws
     wsSort <- getSortByIndex
     let screen = screenForBar bar
     let segments = sepBy "  " [ workspaceBar screen ws wsSort urgents
                               , descriptionBar screen ws
                               , titleBar screen ws title
                               ]
     dbusOutput dbus bar segments

activeScreenId :: WindowSet -> ScreenId
activeScreenId ws = S.screen $ S.current ws

isActiveScreen :: WindowSet -> ScreenId -> Bool
isActiveScreen ws s = activeScreenId ws == s

screenLayout :: ScreenId -> WindowSet -> Maybe (Layout Window)
screenLayout sc w = listToMaybe
                    [ S.layout i
                    | S.Screen i s _ <- S.current w : S.visible w, s == sc
                    ]

screenDescription :: ScreenId -> WindowSet -> String
screenDescription screen ws = maybe "" description (screenLayout screen ws)

workspaceBar :: ScreenId -> WindowSet -> WorkspaceSort -> [Window] -> String
workspaceBar screen ws wsSort urgents =
  sepBy " " $ fmap format $ wsSort $ S.workspaces ws
  where format w = foreground (fg w)
                   $ background (bg w)
                   $ wrapSp
                   $ material
                   $ hideEmpty w
                   $ S.tag w
        fg w | onScreen w = base2
             | hasUrgents w = urgent
             | onAnyScreen w = base03
             | hasWindows w = base1
             | otherwise = base03
        bg w | onScreen w && hasUrgents w = urgent
             | onScreen w && isActiveScreen ws screen = orange
             | onScreen w = blue
             | onAnyScreen w = base00
             | hasWindows w = base02
             | otherwise = base02
        hideEmpty w x = if S.tag w `elem` ["7", "8", "9"]
                           && not (hasWindows w)
                           && not (onAnyScreen w)
                        then ""
                        else x
        hasUrgents w = any (\x -> (== Just (S.tag w)) (S.findTag x ws)) urgents
        onScreen w = S.lookupWorkspace screen ws == Just (S.tag w)
        onAnyScreen w = S.tag w `elem` visibles
        hasWindows w = isJust (S.stack w)
        visibles = fmap (S.tag . S.workspace) (S.current ws : S.visible ws)

titleBar :: ScreenId -> WindowSet -> String -> String
titleBar screen ws title | isActiveScreen ws screen = format title
                         | otherwise                = ""
  where format = foreground base2 . background blue . bold . shorten 128 . wrapSp

descriptionBar :: ScreenId -> WindowSet -> String
descriptionBar screen ws = pad 6 $ screenDescription screen ws

-- https://github.com/google/material-design-icons/blob/master/iconfont/codepoints
material :: WorkspaceId -> String
material x | x == homeWS      = "\xe88a"
           | x == alphaWS     = "\xe400"
           | x == betaWS      = "\xe401"
           | x == mediaWS     = "\xe54d"
           | x == vmWS        = "\xe30b"
           | x == floatWS     = "\xe8aa"
           | x == "7"         = "\xe3d7"
           | x == "8"         = "\xe3d8"
           | x == "9"         = "\xe3d9"
           | x == minimisedWS = "\xe5c3"
           | otherwise        = x

bold :: String -> String
bold = polybarFormat "T" "2"

foreground :: String -> String -> String
foreground = polybarFormat "F"

background :: String -> String -> String
background = polybarFormat "B"

polybarFormat :: String -> String -> String -> String
polybarFormat _   _     ""     = ""
polybarFormat tag value string = "%{" ++ tag ++ value ++ "}"
                                 ++ string
                                 ++ "%{" ++ tag ++ "-}"

wrap :: String -> String -> String -> String
wrap _ _ "" = ""
wrap l r m  = l ++ m ++ r

wrapSp :: String -> String
wrapSp = wrap " " " "

shorten :: Int -> String -> String
shorten n xs | length xs < n = xs
             | otherwise     = take (n - length end) xs ++ end
  where end = "..."

pad :: Int -> String -> String
pad n xs | length xs >= n = xs
         | otherwise      = xs ++ replicate (n - length xs)  ' '

sepBy :: String   -- ^ separator
      -> [String] -- ^ fields to output
      -> String
sepBy sep = intercalate sep . filter (not . null)
