module Text.Icalendar.Parser (
    vcalendar
  ) where


import Control.Applicative
import Text.Parsec
import Text.Parsec.Pos

import Text.Icalendar.Token (Token)
import qualified Text.Icalendar.Token as T
import Text.Icalendar.Types


type Parser = Parsec [Token] ()


vcalendar :: Parser Vcalendar
vcalendar = begin "VCALENDAR" *> (Vcalendar <$> anyToken `manyTill` try (end "VCALENDAR"))


tokenP :: Token -> Parser Token
tokenP t = token show
    (const $ initialPos "token")
    (\t' -> if t == t' then Just t else Nothing)
  <?> show t

begin :: String -> Parser String
begin s = tokenP (T.Begin s) >> return s

end :: String -> Parser String
end s = tokenP (T.End s) >> return s
