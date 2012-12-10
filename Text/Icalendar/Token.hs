module Text.Icalendar.Token (
    Token(..)
  , tokenP
  ) where


import Control.Applicative hiding (many)
import Data.List
import Text.Parsec
import Text.Parsec.ByteString


data Token =
    Begin { beginType :: String }
  | End { endType :: String }
  | Other { otherName :: String, otherParams :: [(String, String)], otherValue :: String }
  deriving (Eq)

instance Show Token where
  show (Begin s) = "BEGIN:" ++ s
  show (End s) = "END:" ++ s
  show (Other k params s) = k ++ showParams params ++ ":" ++ s
    where
      showParams [] = ""
      showParams params' = ';' : intercalate ";" (map showParam params')
      showParam (k', v) = k' ++ "=" ++ v


tokenP :: Parser (SourcePos, Token)
-- TODO support continued lines (beginning with ' ')
-- TODO support escaped newlines ("\\n")
tokenP = do
  pos <- getPosition
  tok <- mkToken
          <$> many (noneOf ";:")
          <*> (char ';' *> param) `manyTill` char ':'
          <*> many (noneOf "\r") <* crlf
  return (pos, tok)

param :: Parser (String, String)
param = (,)
  <$> (many (noneOf "=") <* char '=')
  <*> many (noneOf ";:")


crlf :: Parser String
crlf = string "\r\n"


mkToken :: String -> [(String, String)] -> String -> Token
mkToken "BEGIN" params s | null params = Begin s
                         | otherwise = error "BEGIN can't have params!"
mkToken "END" params s | null params = End s
                       | otherwise = error "END can't have params!"
mkToken name params s = Other name params s
