module Text.Icalendar where

import Text.Parsec
import qualified Text.Parsec.ByteString as PBS

import Text.Icalendar.Parser
import Text.Icalendar.Token
import Text.Icalendar.Types
import Text.Icalendar.Util


parser :: PBS.Parser Vcalendar
parser = vcalendar <.> many tokenP
