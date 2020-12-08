module Advent.CommonParsers
  ( listOfNonNegativeIntegers
  , linesOf
  , nonNegativeInteger
  , word
  , integer
  ) where

import Data.Char (isSpace)
import Text.Parsec (many1, sepEndBy1, eof, option, (<|>))
import Text.Parsec.Char (endOfLine, digit, satisfy, char)
import Text.Parsec.ByteString (Parser)

nonNegativeInteger :: Parser Int
nonNegativeInteger = read <$> many1 digit

integer :: Parser Int
integer = (*) <$> option 1 ((-1 <$ char '-') <|> (1 <$ char '+')) <*> nonNegativeInteger

linesOf :: Parser a -> Parser [a]
linesOf p = sepEndBy1 p endOfLine <* eof

listOfNonNegativeIntegers :: Parser [Int]
listOfNonNegativeIntegers = linesOf nonNegativeInteger

word :: Parser String
word = many1 . satisfy $ not . isSpace
