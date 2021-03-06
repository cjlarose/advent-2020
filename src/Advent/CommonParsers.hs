module Advent.CommonParsers
  ( linesOf
  , natural
  , word
  , integerWithOptionalLeadingSign
  , token
  ) where

import Numeric.Natural (Natural)
import Data.Char (isSpace)
import Text.Parsec (many1, sepEndBy1, eof, option, (<|>))
import Text.Parsec.Char (endOfLine, digit, satisfy, char, space)
import Text.Parsec.ByteString (Parser)

natural :: Parser Natural
natural = read <$> many1 digit

integerWithOptionalLeadingSign :: Integral a => Parser a
integerWithOptionalLeadingSign = (*) <$> option 1 sign <*> (fromIntegral <$> natural)
  where
    sign = (-1 <$ char '-') <|> (1 <$ char '+')

linesOf :: Parser a -> Parser [a]
linesOf p = sepEndBy1 p endOfLine <* eof

word :: Parser String
word = many1 . satisfy $ not . isSpace

token :: Parser String -> Parser String
token p = p <* space
