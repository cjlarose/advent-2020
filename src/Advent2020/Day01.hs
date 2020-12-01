module Advent2020.Day01
  ( solve
  ) where

import Text.Parsec (many1, sepEndBy1, eof)
import Text.Parsec.Char (endOfLine, digit)
import Text.Parsec.ByteString (Parser)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)

entry :: Parser Int
entry = read <$> many1 digit

expenseReport :: Parser [Int]
expenseReport = sepEndBy1 entry endOfLine <* eof

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs (x:xs) = map (\y -> (x, y)) xs ++ pairs xs

productOfSpecialEntries :: [Int] -> Int
productOfSpecialEntries entries = x * y
  where
    (x, y) = head . filter (\(a, b) -> a + b == 2020) . pairs $ entries

printResults :: [Int] -> (String, String)
printResults entries = (part1, part2)
  where
    part1 = show . productOfSpecialEntries $ entries
    part2 = "not yet implemented"

solve :: IO (Either String (String, String))
solve = withSuccessfulParse expenseReport printResults <$> getProblemInputAsByteString 1
