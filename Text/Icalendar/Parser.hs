module Text.Icalendar.Parser (
    vcalendar
  ) where


import Control.Applicative
import Text.Parsec

import Text.Icalendar.Token
import Text.Icalendar.Types


type Parser = Parsec [Token] ()


vcalendar :: Parser Vcalendar
vcalendar = begin "VCALENDAR" *> (Vcalendar <$> anyToken `manyTill` try (end "VCALENDAR"))

begin :: String -> Parser Token
begin s = do
  object <- anyToken
  case object of
    Begin s' | s == s' -> return object
    _ -> parserFail $ "expected BEGIN:" ++ s

end :: String -> Parser Token
end s = do
  object <- anyToken
  case object of
    End s' | s == s' -> return object
    _ -> parserFail $ "expected END:" ++ s ++ ", got " ++ show object
