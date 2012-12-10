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
  deriving (Eq)

instance Show Token where
  show (Begin s) = "BEGIN:" ++ s
  show (End s) = "END:" ++ s
  show (Other p s) = p ++ ":" ++ s


tokenP :: Parser (SourcePos, Token)
-- TODO support continued lines (beginning with ' ')
-- TODO support escaped newlines ("\\n")
tokenP = do
  pos <- getPosition
  tok <- mkToken <$> (many (noneOf ":") <* char ':') <*> many (noneOf "\r") <* crlf
  return (pos, tok)


crlf :: Parser String
crlf = string "\r\n"


mkToken :: String -> String -> Token
mkToken "BEGIN" s = Begin s
mkToken "END" s = End s
mkToken prefix s = Other prefix s
