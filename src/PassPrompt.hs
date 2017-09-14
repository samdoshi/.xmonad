{-# LANGUAGE LambdaCase #-}

module PassPrompt ( sendUsernamePasswordPrompt
                  ) where

import           Control.Monad       (join)
import           Control.Monad.Trans (liftIO)
import           Data.List           (find)
import           System.Exit         (ExitCode (ExitFailure, ExitSuccess))
import           System.Process      (readProcessWithExitCode)
import           Text.Read           (readEither, readMaybe)

import           Graphics.X11.Types  (noModMask, xK_Tab)
import           XMonad.Core         (X, installSignalHandlers,
                                      uninstallSignalHandlers)
import           XMonad.Prompt       (XPConfig, XPrompt, alwaysHighlight,
                                      commandToComplete, getNextCompletion,
                                      mkXPrompt, nextCompletion,
                                      searchPredicate, showXPrompt)
import           XMonad.Util.Paste   (pasteString, sendKey)
import           XMonad.Util.Run     (safeSpawn)

type Predicate = String -> String -> Bool

getPassCompl :: Passwords -> Predicate -> String -> IO [String]
getPassCompl compls p s = return $ filter (p s) $ fmap snd compls

type Passwords = [(String, String)]
type PasswordFunction = Passwords -> (String -> X ())

newtype Pass = Pass String

instance XPrompt Pass where
  showXPrompt       (Pass prompt) = prompt ++ ": "
  commandToComplete _ c           = c
  nextCompletion      _           = getNextCompletion

-- | Make different password prompts
mkPassPrompt :: String -> PasswordFunction -> XPConfig -> X ()
mkPassPrompt promptLabel passwordFunction xpconfig = do
  passwords <- liftIO getPasswords
  case passwords of
    Just p -> mkXPrompt (Pass promptLabel)
                        xpconfig { alwaysHighlight = True }
                        (getPassCompl p $ searchPredicate xpconfig)
                        (passwordFunction p)
    Nothing -> unlockPasswords

-- | A prompt to send the username and password to the current window
sendUsernamePasswordPrompt :: XPConfig -> X ()
sendUsernamePasswordPrompt = mkPassPrompt
                             "Send username / password"
                             sendUsernamePasswordFn

-- | Given a password, find the UUID, retrieve the password and send it to a window
sendUsernamePasswordFn :: PasswordFunction
sendUsernamePasswordFn passwords passLabel = do
  let uuid = fst <$> find (\(_, name) -> name == passLabel) passwords
  case uuid of
    Nothing    -> pure ()
    Just uuid' -> sendUsernamePassword uuid'

-- | Ask 1passkell to unlock passwords
unlockPasswords :: X ()
unlockPasswords = safeSpawn "1passkell" ["unlock"]

-- | Send "username", <TAB>, "password" to current window
sendUsernamePassword :: String -> X ()
sendUsernamePassword uuid =
  liftIO (getUsernamePassword uuid) >>= \case
    Nothing  -> pure ()
    Just (username, password) -> do pasteString username
                                    sendKey noModMask xK_Tab
                                    pasteString password

-- | Retrieve a username and password from a UUID
getUsernamePassword :: String -> IO (Maybe (String, String))
getUsernamePassword uuid = do
  out <- run1Passkell ["print", "-m", uuid]
  case join $ fmap readEither out of
    Left _  -> pure Nothing
    Right o -> pure $ Just o

-- | Retrieve the list of password from 1passkell
getPasswords :: IO (Maybe Passwords)
getPasswords = do
  out <- run1Passkell ["list", "--nw", "-m"] >>= \case
    Right s -> pure $ Just s
    Left _  -> pure Nothing
  return $ join $ fmap readMaybe out

-- | Run the 1passkell command
run1Passkell :: [String]
             -> IO (Either String String)
run1Passkell opts =
  -- https://github.com/xmonad/xmonad/issues/115
  do uninstallSignalHandlers
     (exitCode, stdout, stderr) <- readProcessWithExitCode "1passkell"
                                   opts
                                   []
     installSignalHandlers
     pure $ case exitCode of
       ExitSuccess   -> Right stdout
       ExitFailure _ -> Left stderr
