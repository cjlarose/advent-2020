module Advent2020.Day01
  ( solve
  , subsetSums
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map (Map, (!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (foldl')
import Control.Monad (guard)
import Debug.Trace (traceShowId, traceShow)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (listOfNonNegativeIntegers)

subsetSums :: Int -> Set Int -> [Set Int]
subsetSums k xs = subsetsForSum ! (xs, k)
  where
    prefixes = map (`Set.take` xs) [0..Set.size xs]
    init = Map.fromList . map (\prefix -> ((prefix, 0), [Set.empty])) $ prefixes
    subsetsForSum = foldl' f init [(prefix, target) | prefix <- prefixes, target <- [1..k]]
    f :: Map (Set Int, Int) [Set Int] -> (Set Int, Int) -> Map (Set Int, Int) [Set Int]
    f acc (prefix, target) =
      case Set.maxView prefix of
        Nothing -> Map.insert (prefix, target) [] acc
        Just (x, rest) -> if x > target
                          then Map.insert (prefix, target) (acc ! (rest, target)) acc
                          else let l = acc ! (rest, target)
                                   r = map (Set.insert x) $ acc ! (rest, target - x)
                               in Map.insert (prefix, target) (l ++ r) acc

productOfSpecialPair :: [Set Int] -> Int
productOfSpecialPair subsets = x * y
  where
    [x, y] = Set.toList . head . filter (\xs -> Set.size xs == 2) $ subsets

productOfSpecialTriple :: [Set Int] -> Int
productOfSpecialTriple subsets = x * y * z
  where
    [x, y, z] = Set.toList . head . filter (\xs -> Set.size xs == 3) $ subsets

printResults :: [Int] -> PuzzleAnswerPair
printResults entries = PuzzleAnswerPair (part1, part2)
  where
    subsets = subsetSums 2020 . Set.fromList $ entries
    part1 = show . productOfSpecialPair $ subsets
    part2 = show . productOfSpecialTriple $ subsets

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse listOfNonNegativeIntegers printResults <$> getProblemInputAsByteString 1
