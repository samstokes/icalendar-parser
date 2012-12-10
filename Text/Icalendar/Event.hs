module Text.Icalendar.Event (
    Event(..)
  , parseDate
  , parseDateTime
  ) where


import Data.Maybe
import Data.Time
import System.Locale


data Event = Event {
    eventStart :: ZonedTime
  , eventEnd :: ZonedTime
  , eventSummary :: String
  , eventProperties :: [(String, ([(String, String)], String))]
  } deriving (Show)


parseDateTime :: Maybe TimeZone -> String -> ZonedTime
parseDateTime Nothing s | last s == 'Z' = inTimeZone utc $ fromMaybe (error $ "malformed time " ++ s) $ parseTime defaultTimeLocale "%Y%m%dT%H%M%sZ" s
                        | otherwise = error $ "Can't determine time zone for " ++ s
parseDateTime (Just tz) s = inTimeZone tz $ fromMaybe (error $ "malformed time " ++ s) $ parseTime defaultTimeLocale "%Y%m%dT%H%M%s" s

parseDate :: Maybe TimeZone -> String -> ZonedTime
parseDate Nothing s = inTimeZone utc $ fromMaybe (error $ "malformed date " ++ s) $ parseTime defaultTimeLocale "%Y%m%d" s
parseDate _ s = error $ "Not sure how to handle date with time zone: " ++ s

inTimeZone :: TimeZone -> LocalTime -> ZonedTime
inTimeZone = flip ZonedTime
