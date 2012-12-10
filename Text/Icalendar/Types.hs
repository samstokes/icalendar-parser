module Text.Icalendar.Types (
    Vcalendar(..)
  , Property(..)
  , Component(..)
  , isEvent
  , vcalEvents
  ) where


data Vcalendar = Vcalendar {
    vcalProperties :: [Property]
  , vcalComponents :: [Component]
  } deriving (Show)

data Property = Property {
    propertyName :: String
  , propertyParams :: [(String, String)]
  , propertyValue :: String
  } deriving (Show)

data Component = Component {
    componentType :: String
  , componentProperties :: [Property]
  , componentSubcomponents :: [Component]
  } deriving (Show)


isEvent :: Component -> Bool
isEvent (Component "VEVENT" _ _) = True
isEvent _ = False


vcalEvents :: Vcalendar -> [Component]
vcalEvents = filter isEvent . vcalComponents
