module Text.Icalendar.Types (
    Vcalendar(..)
  , Property(..)
  , Component(..)
  ) where


data Vcalendar = Vcalendar [Property] [Component]
  deriving (Show)

data Property = Property String String
  deriving (Show)

data Component = Component String [Property] [Component]
  deriving (Show)
