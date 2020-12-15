module Advent2020.Day15
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
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
    mostRecentIndex :: Map Int [Int]
    mostRecentIndex = Map.fromList . zip inits . map pure $ [0..]

    go :: Int -> Int -> Map Int [Int] -> Int
    go i last acc
      | i == k = last
      | otherwise = go (i + 1) next newAcc
          where
            next = case acc ! last of
                     (j:k:_) -> j - k
                     _ -> 0
            newAcc = Map.alter (Just . maybe [i] (\(x:_) -> [i, x])) next acc

printResults :: [Int] -> PuzzleAnswerPair
printResults starting = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . spokenAt 2020 $ starting
    part2 = show . spokenAt 30000000 $ starting

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 15
