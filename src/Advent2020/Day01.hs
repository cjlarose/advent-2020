module Advent2020.Day01
  ( solve
  ) where

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (listOfNonNegativeIntegers)

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs (x:xs) = map (\y -> (x, y)) xs ++ pairs xs

triples :: [Int] -> [(Int, Int, Int)]
triples [] = []
triples (x:xs) = withHead ++ withoutHead
  where
    withHead = map (\(y, z) -> (x, y, z)) . pairs $ xs
    withoutHead = triples xs

productOfSpecialPair :: [Int] -> Int
productOfSpecialPair entries = x * y
  where
    (x, y) = head . filter (\(a, b) -> a + b == 2020) . pairs $ entries

productOfSpecialTriple :: [Int] -> Int
productOfSpecialTriple entries = x * y * z
  where
    (x, y, z) = head . filter (\(a, b, c) -> a + b + c == 2020) . triples $ entries

printResults :: [Int] -> PuzzleAnswerPair
printResults entries = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . productOfSpecialPair $ entries
    part2 = show . productOfSpecialTriple $ entries

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse listOfNonNegativeIntegers printResults <$> getProblemInputAsByteString 1
