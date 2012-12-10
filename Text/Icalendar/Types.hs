module Text.Icalendar.Types (
    Vcalendar(..)
  ) where


import Text.Icalendar.Token


data Vcalendar = Vcalendar [Token]
  deriving (Show)
