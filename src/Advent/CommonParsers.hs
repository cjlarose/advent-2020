module Advent.CommonParsers
  ( listOfNonNegativeIntegers
  , linesOf
  , nonNegativeInteger
  , word
  , integerWithOptionalLeadingSign
  , token
  ) where

import Data.Char (isSpace)
import Text.Parsec (many1, sepEndBy1, eof, option, (<|>))
import Text.Parsec.Char (endOfLine, digit, satisfy, char, space)
import Text.Parsec.ByteString (Parser)

nonNegativeInteger :: Parser Int
nonNegativeInteger = read <$> many1 digit

integerWithOptionalLeadingSign :: Parser Int
integerWithOptionalLeadingSign = (*) <$> option 1 sign <*> nonNegativeInteger
  where
    sign = (-1 <$ char '-') <|> (1 <$ char '+')

linesOf :: Parser a -> Parser [a]
linesOf p = sepEndBy1 p endOfLine <* eof

listOfNonNegativeIntegers :: Parser [Int]
listOfNonNegativeIntegers = linesOf nonNegativeInteger

word :: Parser String
word = many1 . satisfy $ not . isSpace

token :: Parser String -> Parser String
token p = p <* space
