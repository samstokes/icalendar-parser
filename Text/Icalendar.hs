module Text.Icalendar (
    module Text.Icalendar.Event
  , module Text.Icalendar.Types
  , events
  , parser
  ) where

import Data.Maybe
import Data.Time
import Text.Parsec
import qualified Text.Parsec.ByteString as PBS

import Text.Icalendar.Event
import Text.Icalendar.Parser
import Text.Icalendar.Token
import Text.Icalendar.Types
import Text.Icalendar.Util


parser :: PBS.Parser Vcalendar
parser = vcalendar <.> many tokenP


events :: Vcalendar -> [Event]
events = map nicerEvent . vcalEvents


nicerEvent :: Component -> Event
nicerEvent evt | isEvent evt = Event {
    eventStart = start
  , eventEnd = end
  , eventSummary = summary
  , eventProperties = properties
  } where
    properties = map prop2tuple $ componentProperties evt
    prop2tuple (Property k params v) = (k, (params, v))
    start = uncurry parseDateTimeField $ properties <!> "DTSTART"
    end = uncurry parseDateTimeField $ properties <!> "DTEND"
    summary = snd $ properties <!> "SUMMARY"
nicerEvent c = error $ "Unexpected component type " ++ componentType c

parseDateTimeField :: [(String, String)] -> String -> ZonedTime
parseDateTimeField params s = if isDate then parseDate Nothing s else parseDateTime tz s
  where
    isDate = Just "DATE" == lookup "VALUE" params
    tz | last s == 'Z' = Nothing
       | otherwise = Just . zone $ fromMaybe (error $ "Can't determine time zone for " ++ s) $ lookup "TZID" params
    zone _ = read "PST" :: TimeZone -- TODO!

(<!>) :: [(String, v)] -> String -> v
l <!> k = fromMaybe (error $ "missing property " ++ k) $ lookup k l
