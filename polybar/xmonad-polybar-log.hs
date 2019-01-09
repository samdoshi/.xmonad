{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (forM_, forever)
import           GHC.Generics       (Generic)
import           System.IO          (BufferMode (LineBuffering), hSetBuffering,
                                     stdout)

import           DBus               (ObjectPath, Signal, fromVariant,
                                     signalBody)
import           DBus.Client        (MatchRule, addMatch, connectSession,
                                     matchAny, matchDestination, matchInterface,
                                     matchMember, matchPath, matchSender)
import           Options.Generic    (ParseRecord, getRecord)

data Mode = Left
          | Right
          | One
          deriving (Generic, Show)

instance ParseRecord Mode

pathForMode :: Mode -> ObjectPath
pathForMode Main.Left  = "/com/samdoshi/xmonad/left"
pathForMode Main.Right = "/com/samdoshi/xmonad/right"
pathForMode Main.One   = "/com/samdoshi/xmonad/one"

matchRule :: Mode -> MatchRule
matchRule mode = matchAny { matchPath        = Just $ pathForMode mode
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

-- https://ghc.haskell.org/trac/ghc/ticket/7325
-- https://stackoverflow.com/questions/31845305/does-threaddelay-maxbound-int-trip-a-ghc-bug-or-what
delay :: Int
delay = 10000000000

main :: IO ()
main = do
  mode <- getRecord "xmonad-polybar-log"
  hSetBuffering stdout LineBuffering
  client <- connectSession
  _ <- addMatch client (matchRule mode) printMessage
  forever (threadDelay delay)
