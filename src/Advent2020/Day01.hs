module Advent2020.Day01
  ( solve
  ) where

import Control.Monad (guard)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (listOfNonNegativeIntegers)

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs (x:xs) = map (\y -> (x, y)) xs ++ pairs xs

productOfSpecialPair :: [Int] -> Int
productOfSpecialPair entries = x * y
  where
    (x, y) = head . filter (\(a, b) -> a + b == 2020) . pairs $ entries

productOfSpecialTriple :: [Int] -> Int
productOfSpecialTriple entries = x * y * z
  where
    (x, y, z) = head $ do
      (a, b) <- pairs entries
      guard $ a + b < 2020
      let rest = 2020 - a - b
      c <- filter (rest ==) entries
      pure (a, b, c)

printResults :: [Int] -> PuzzleAnswerPair
printResults entries = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . productOfSpecialPair $ entries
    part2 = show . productOfSpecialTriple $ entries

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse listOfNonNegativeIntegers printResults <$> getProblemInputAsByteString 1
