module Advent2020.Day01
  ( solve
  ) where

import qualified Data.Set as Set
import Control.Monad (guard)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf, integerWithoutLeadingSign)
import Advent.ListUtils (subsetsOfCardinalityTwo)

productOfSpecialPair :: [Int] -> Int
productOfSpecialPair entries = x * y
  where
    entrySet = Set.fromList entries
    (x, y) = head $ do
      a <- entries
      let b = 2020 - a
      guard $ b `Set.member` entrySet
      pure (a, b)

productOfSpecialTriple :: [Int] -> Int
productOfSpecialTriple entries = x * y * z
  where
    entrySet = Set.fromList entries
    (x, y, z) = head $ do
      (a, b) <- subsetsOfCardinalityTwo entries
      let c = 2020 - a - b
      guard $ c `Set.member` entrySet
      pure (a, b, c)

printResults :: [Int] -> PuzzleAnswerPair
printResults entries = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . productOfSpecialPair $ entries
    part2 = show . productOfSpecialTriple $ entries

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse (linesOf integerWithoutLeadingSign) printResults <$> getProblemInputAsByteString 1
