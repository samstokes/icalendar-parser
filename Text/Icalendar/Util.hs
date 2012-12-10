{-# LANGUAGE FlexibleContexts #-}

module Text.Icalendar.Util (
    (<.>)
  ) where

import Data.Functor.Identity
import Text.Parsec

(<.>) :: Stream s' Identity t => Parsec s' () s'' -> Parsec s () s' -> Parsec s () s''
parser <.> lexer = do
  parsedTokens <- lexer
  case parse parser "tokens" parsedTokens of
    Right v -> return v
    Left err -> parserFail $ show err
