module Text.Icalendar.Types (
    Vcalendar(..)
  , Property(..)
  , Component(..)
  ) where


data Vcalendar = Vcalendar {
    vcalProperties :: [Property]
  , vcalComponents :: [Component]
  } deriving (Show)

data Property = Property {
    propertyName :: String
  , propertyValue :: String
  } deriving (Show)

data Component = Component {
    componentType :: String
  , componentProperties :: [Property]
  , componentSubcomponents :: [Component]
  } deriving (Show)
