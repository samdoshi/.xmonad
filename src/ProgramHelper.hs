module ProgramHelper ( isBrowser
                     , isBrowser'
                     , isEmacs
                     , isEmacs'
                     , isTerminal
                     , isTerminal'
                     , runBrowser
                     , runEmacs
                     , runTerminal
                     ) where

import           Control.Monad.Trans (MonadIO)
import           XMonad.Core         (Query)
import           XMonad.ManageHook   (className)
import           XMonad.Util.Run     (safeSpawn)

isBrowser :: String -> Bool
isBrowser "Chromium" = True
isBrowser _          = False

isBrowser' :: Query Bool
isBrowser' = toClassNameQuery isBrowser

isEmacs :: String -> Bool
isEmacs "Emacs" = True
isEmacs _       = False

isEmacs' :: Query Bool
isEmacs' = toClassNameQuery isEmacs

isTerminal :: String -> Bool
isTerminal "URxvt" = True
isTerminal _       = False

isTerminal' :: Query Bool
isTerminal' = toClassNameQuery isTerminal

toClassNameQuery :: (String -> Bool) -> Query Bool
toClassNameQuery f = fmap f className

runBrowser :: MonadIO m => m ()
runBrowser = safeSpawn "chromium" ["--force-device-scale-factor=1.75"]

runEmacs :: MonadIO m => m ()
runEmacs = safeSpawn "emacsclient" ["--create-frame"]

runTerminal :: MonadIO m => m()
runTerminal = safeSpawn "urxvt" []
