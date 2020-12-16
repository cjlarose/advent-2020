module Advent2020.Day15
  ( solve
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, endOfLine)
import Text.Parsec (sepBy1, eof)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integerWithOptionalLeadingSign)

inputParser :: Parser [Int]
inputParser = sepBy1 integerWithOptionalLeadingSign (char ',') <* endOfLine <* eof

spokenAt :: Int -> [Int] -> Int
spokenAt k inits = go (length inits) (last inits) mostRecentIndex
  where
    mostRecentIndex :: IntMap Int
    mostRecentIndex = IntMap.fromList . zip (init inits) $ [0..]

    go :: Int -> Int -> IntMap Int -> Int
    go i last acc
      | i == k = last
      | otherwise = go (i + 1) next newAcc
          where
            (oldVal, newAcc) = IntMap.insertLookupWithKey (\_ v _ -> v) last (i - 1) acc
            next = maybe 0 (\j -> i - j - 1) oldVal

printResults :: [Int] -> PuzzleAnswerPair
printResults starting = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . spokenAt 2020 $ starting
    part2 = show . spokenAt 30000000 $ starting

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 15
