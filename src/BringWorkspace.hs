module BringWorkspace ( bringWorkspaceWindow
                      ) where

import           Control.Monad.State       (gets)
import           Data.List                 (nub)
import           Graphics.X11.Types        (Window)
import           XMonad.Actions.GridSelect (GSConfig, gridselect)
import           XMonad.Core               (WindowSet, WorkspaceId, X,
                                            windowset)
import           XMonad.Operations         (focus, windows)
import qualified XMonad.StackSet           as W
import           XMonad.Util.NamedWindows  (getName)


bringWorkspaceWindow :: WorkspaceId -> GSConfig Window -> X ()
bringWorkspaceWindow ws = withWorkspaceWindow ws $ \w -> do
  windows (bringWindow w)
  focus w

bringWindow :: Window -> WindowSet -> WindowSet
bringWindow w ws = W.shiftWin (W.currentTag ws) w ws

withWorkspaceWindow :: WorkspaceId
                    -> (Window -> X ())
                    -> GSConfig Window
                    -> X ()
withWorkspaceWindow ws callback conf = do
    windowSet <- gets windowset
    mbWindow <- windowMap (workspaceWindows ws windowSet) >>= gridselect conf
    case mbWindow of
        Just w -> callback w
        Nothing -> pure ()

workspaceWindows :: WorkspaceId -> WindowSet -> [Window]
workspaceWindows ws set = nub $ concatMap (W.integrate' . W.stack) workspace
  where workspace = filter (\w -> W.tag w == ws) (W.workspaces set)

windowMap :: [Window] -> X [(String, Window)]
windowMap = mapM keyValuePair
 where keyValuePair w = flip (,) w <$> decorateName w
       decorateName w = show <$> getName w
