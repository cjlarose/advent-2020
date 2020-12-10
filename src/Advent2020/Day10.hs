module Advent2020.Day10
  ( solve
  ) where

import Data.List (sort)
import Text.Parsec.ByteString (Parser)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integer, linesOf)
import Advent.ListUtils (pairs, subLists)

inputParser :: Parser [Int]
inputParser = linesOf integer

joltageDifferences :: [Int] -> (Int, Int)
joltageDifferences xs = (diffsOf 1, diffsOf 3 + 1)
  where
    diffsOf k = length . filter (== k) $ diffs
    diffs :: [Int]
    diffs = map (\[x, y] -> y - x) . filter (\l -> length l == 2) . subLists $ (0:sort xs)

printResults :: [Int] -> PuzzleAnswerPair
printResults joltages = PuzzleAnswerPair (part1, part2)
  where
    (diffOf1, diffOf3) = joltageDifferences joltages
    part1 = show $ diffOf1 * diffOf3
    part2 = "not implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 10
