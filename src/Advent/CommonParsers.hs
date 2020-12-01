module Advent.CommonParsers
  ( listOfNonNegativeIntegers
  ) where

import Text.Parsec (many1, sepEndBy1, eof)
import Text.Parsec.Char (endOfLine, digit)
import Text.Parsec.ByteString (Parser)

nonNegativeInteger :: Parser Int
nonNegativeInteger = read <$> many1 digit

listOfNonNegativeIntegers :: Parser [Int]
listOfNonNegativeIntegers = sepEndBy1 nonNegativeInteger endOfLine <* eof

