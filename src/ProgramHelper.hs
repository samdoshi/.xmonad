module ProgramHelper ( defaultTerminal
                     , isBrowser
                     , isBrowser'
                     , isEmacs
                     , isEmacs'
                     , isTerminal
                     , isTerminal'
                     , isMutt
                     , isMutt'
                     , runBrowser
                     , runEmacs
                     , runTerminal
                     , runMutt
                     ) where

import           Control.Monad.Trans (MonadIO)
import           XMonad.Core         (Query)
import           XMonad.ManageHook   (className)
import           XMonad.Util.Run     (safeSpawn, unsafeSpawn)

defaultTerminal :: String
defaultTerminal = "termite"

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
isTerminal "Termite" = True
isTerminal "URxvt"   = True
isTerminal _         = False

isTerminal' :: Query Bool
isTerminal' = toClassNameQuery isTerminal

isMutt :: String -> Bool
isMutt "Mutt" = True
isMutt _      = False

isMutt' :: Query Bool
isMutt' = toClassNameQuery isMutt

toClassNameQuery :: (String -> Bool) -> Query Bool
toClassNameQuery f = fmap f className

runBrowser :: MonadIO m => m ()
runBrowser = safeSpawn "chromium" [ "--force-device-scale-factor=1.75"
                                  , "--disk-cache-dir=/tmp/cache/chromium"
                                  ]

runEmacs :: MonadIO m => m ()
runEmacs = safeSpawn "emacsclient" ["--create-frame"]

runTerminal :: MonadIO m => m ()
runTerminal = safeSpawn defaultTerminal []

runMutt :: MonadIO m => m ()
runMutt = runInTerminalWithClass "mutt" "Mutt"

runInTerminalWithClass :: MonadIO m => String -> String -> m ()
runInTerminalWithClass cmd cls =
  unsafeSpawn $ defaultTerminal ++ " --exec=\"zsh -ic " ++ cmd ++ "\" --class=" ++ cls
