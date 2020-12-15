module Advent2020.Day15
  ( solve
  ) where

import Data.List (elemIndex)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, endOfLine)
import Text.Parsec (sepBy1, eof)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integerWithOptionalLeadingSign)

inputParser :: Parser [Int]
inputParser = sepBy1 integerWithOptionalLeadingSign (char ',') <* endOfLine <* eof

spokenAt :: Int -> [Int] -> Int
spokenAt k inits = go (length inits) (reverse inits)
  where
    go :: Int -> [Int] -> Int
    go i (last:prevs)
      | i == k = last
      | otherwise = go (i + 1) $ next : last : prevs
          where
            next = case elemIndex last prevs of
              Nothing -> 0
              Just j -> j + 1

printResults :: [Int] -> PuzzleAnswerPair
printResults starting = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . spokenAt 2020 $ starting
    part2 = "not yet implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 15
