module Text.Icalendar.Token (
    Token(..)
  , tokenP
  ) where


import Control.Applicative hiding (many)
import Text.Parsec
import Text.Parsec.ByteString


data Token =
    Begin String
  | End String
  | Other String String
  deriving (Show)


tokenP :: Parser Token
tokenP = mkToken <$> (many (noneOf ":") <* char ':') <*> many (noneOf "\r") <* crlf


crlf :: Parser String
crlf = string "\r\n"


mkToken :: String -> String -> Token
mkToken "BEGIN" s = Begin s
mkToken "END" s = End s
mkToken prefix s = Other prefix s
