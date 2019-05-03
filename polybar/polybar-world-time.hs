module Main (main) where

import           Control.Concurrent  (threadDelay)
import           Control.Monad       (forever)
import           Data.List           (intercalate)
import           System.IO           (BufferMode (LineBuffering), hSetBuffering,
                                      stdout)

import           Data.Time           (LocalTime (LocalTime), NominalDiffTime,
                                      TimeOfDay (..), UTCTime,
                                      ZonedTime (ZonedTime), addUTCTime,
                                      diffUTCTime, getCurrentTime, getZonedTime,
                                      localTimeToUTC, toGregorian, utc,
                                      utcToLocalTime)
import           Data.Time.Zones     (TZ, timeZoneForUTCTime, utcToLocalTimeTZ)
import qualified Data.Time.Zones.All as Zones


-- * Time zones

zones :: [(String, TZ)]
zones = [ ("San Francisco", l Zones.America__Los_Angeles)
        , ("New York",      l Zones.America__New_York)
        , ("London",        l Zones.Europe__London)
        , ("Paris",         l Zones.Europe__Paris)
        , ("Tokyo",         l Zones.Asia__Tokyo)
        , ("Auckland",      l Zones.Pacific__Auckland)
        ]
  where l = Zones.tzByLabel

zonedTimeForTZ :: TZ -> UTCTime -> ZonedTime
zonedTimeForTZ tz t = ZonedTime ztlt zttz
  where ztlt = utcToLocalTimeTZ tz t
        zttz = timeZoneForUTCTime tz t

-- add ordinal, e.g. 1 -> 1st, 2 -> 2nd, etc
th :: Int -> String
th= foldr f "th" . show
  where f '1' "th"    = "1st"
        f '2' "th"    = "2nd"
        f '3' "th"    = "3rd"
        f '1' [x,_,_] = '1':x:"th"
        f a s=        a:s

-- zero pad 1 digit number to 2
zp :: Int -> String
zp i | i >= 0 && i < 10 = "0" ++ show i
     | otherwise = show i

formatTime :: ZonedTime -> String
formatTime (ZonedTime (LocalTime _ tod) _) = h ++ ":" ++ m
  where h = zp $ todHour tod
        m = zp $ todMin tod

formatDay :: ZonedTime -> String
formatDay (ZonedTime (LocalTime ld _) _) = day
  where day = let (_, _, d) = toGregorian ld in th d


-- * Polybar output

coloursForZonedTime :: ZonedTime -> (String, String)
coloursForZonedTime (ZonedTime (LocalTime _ (TimeOfDay h _ _)) _)
  | day = (base2, base03)
  | twilight = (base01, base2)
  | otherwise = (base03, base2)
  where day = h >= 7 && h <= 17
        twilight = (h >= 5 && h < 7) || (h > 17 && h <= 19)
        base03  = "#002b36"
        -- base02  = "#073642"
        base01  = "#586e75"
        -- base00  = "#657b83"
        -- base0   = "#839496"
        -- base1   = "#93a1a1"
        base2   = "#eee8d5"
        -- base3   = "#fdf6e3"

wrap :: String -> String -> String -> String
wrap _ _ "" = ""
wrap l r m  = l ++ m ++ r

wrapSp :: String -> String
wrapSp = wrap " " " "

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

polybarTime :: String -> ZonedTime -> String
polybarTime n zt = background bg
                   $ foreground fg
                   $ wrapSp
                   $ n ++ ": " ++ formatDay zt ++ " " ++ bold (formatTime zt)
  where (bg, fg) = coloursForZonedTime zt

polybarTimes :: UTCTime -> String
polybarTimes t = sepBy "  " $ fmap f zones
  where f (n, z) = polybarTime n $ zonedTimeForTZ z t
        sepBy sep = intercalate sep . filter (not . null)


-- * Sleep till next minute

-- from time >= 1.9
addLocalTime :: NominalDiffTime -> LocalTime -> LocalTime
addLocalTime x = utcToLocalTime utc . addUTCTime x . localTimeToUTC utc

-- from time >= 1.9
diffLocalTime :: LocalTime -> LocalTime -> NominalDiffTime
diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)

microsecondsToNextMinute :: LocalTime -> Int
microsecondsToNextMinute now@(LocalTime ld ltod) =
  round (realToFrac $ 1000000 * diff :: Double)
  where before = LocalTime ld (ltod { todSec = 0 })
        after = addLocalTime 60 before
        diff = diffLocalTime after now

sleepToNextMinute :: IO ()
sleepToNextMinute = do
  ZonedTime now _ <- getZonedTime
  threadDelay $ microsecondsToNextMinute now

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  forever $ do
    t <- getCurrentTime
    putStrLn $ polybarTimes t
    sleepToNextMinute
