{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (forM_, forever)
import           System.IO

import           DBus               (Signal, fromVariant, signalBody)
import           DBus.Client        (MatchRule, addMatch, connectSession,
                                     matchAny, matchDestination, matchInterface,
                                     matchMember, matchPath, matchSender)

matchRule :: MatchRule
matchRule = matchAny { matchPath        = Just "/com/samdoshi/xmonad/log"
                     , matchSender      = Nothing
                     , matchDestination = Nothing
                     , matchInterface   = Just "com.samdoshi.xmonad"
                     , matchMember      = Just "Update"
                     }

printMessage :: Signal -> IO ()
printMessage s = forM_ (signalBody s) $ \v ->
  case fromVariant v of
    Just str -> putStrLn str
    Nothing  -> putStrLn "Unknown"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  client <- connectSession
  _ <- addMatch client matchRule printMessage
  forever (threadDelay maxBound)
