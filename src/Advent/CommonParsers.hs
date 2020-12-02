module Advent.CommonParsers
  ( listOfNonNegativeIntegers
  , linesOf
  , nonNegativeInteger
  ) where

import Text.Parsec (many1, sepEndBy1, eof)
import Text.Parsec.Char (endOfLine, digit)
import Text.Parsec.ByteString (Parser)

nonNegativeInteger :: Parser Int
nonNegativeInteger = read <$> many1 digit

linesOf :: Parser a -> Parser [a]
linesOf p = sepEndBy1 p endOfLine <* eof

listOfNonNegativeIntegers :: Parser [Int]
listOfNonNegativeIntegers = linesOf nonNegativeInteger

