module Text.Icalendar.Parser (
    vcalendar
  ) where


import Control.Applicative hiding ((<|>))
import Control.Monad
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim

import Text.Icalendar.Token (Token)
import qualified Text.Icalendar.Token as T
import Text.Icalendar.Types


type Parser = Parsec [Token] ()


vcalendar :: Parser Vcalendar
vcalendar = comp2cal <$> component (Just "VCALENDAR")
  where comp2cal (Component "VCALENDAR" props comps) = Vcalendar props comps
        comp2cal (Component typ _ _) = error $ "Unexpected component type " ++ typ

property :: Parser Property
property = tok2prop <$> anyToken
  where tok2prop (T.Other k v) = Property k v
        tok2prop t = error $ "Unexpected " ++ show t

component :: Maybe String -> Parser Component
component maybeType = do
  typ <- maybe beginAny begin maybeType
  Component typ
    <$> property `manyTill` lookAhead (beginAny <|> end typ)
    <*> component Nothing `manyTill`
    try (end typ)


satisfy :: (Token -> Bool) -> String -> Parser Token
satisfy f description = token show
    (const $ initialPos "token")
    (\t' -> if f t' then Just t' else Nothing)
  <?> description

tokenP :: Token -> Parser Token
tokenP t = satisfy (== t) (show t)

begin :: String -> Parser String
begin s = tokenP (T.Begin s) >> return s

beginAny :: Parser String
beginAny = liftM unBegin $ satisfy isBegin "BEGIN:"
  where
    isBegin (T.Begin _) = True
    isBegin _ = False
    unBegin (T.Begin s) = s
    unBegin _ = error "Should never get here"

end :: String -> Parser String
end s = tokenP (T.End s) >> return s
