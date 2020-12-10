module Advent2020.Day10
  ( solve
  ) where

import Data.List (sort, foldl', inits)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, viewr, ViewR((:>)), (|>))
import qualified Data.Sequence as Seq
import Text.Parsec.ByteString (Parser)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integer, linesOf)
import Advent.ListUtils (subLists)

inputParser :: Parser [Int]
inputParser = linesOf integer

joltageDifferences :: [Int] -> (Int, Int)
joltageDifferences xs = (diffsOf 1, diffsOf 3 + 1)
  where
    diffsOf k = length . filter (== k) $ diffs
    diffs :: [Int]
    diffs = map (\[x, y] -> y - x) . filter (\l -> length l == 2) . subLists $ (0:sort xs)

validArrangements :: [Int] -> Int
validArrangements xs = waysToGetTo ! (desiredJoltage, joltages |> desiredJoltage)
  where
    joltages = Seq.fromList xs
    desiredJoltage = maximum joltages + 3

    waysToGetTo :: Map (Int, Seq Int) Int
    waysToGetTo = foldl' f Map.empty $ [(j, Seq.fromList prefix) |  prefix <- inits (xs ++ [desiredJoltage]), j <- [0..desiredJoltage]]
      where
        f :: Map (Int, Seq Int) Int -> (Int, Seq Int) -> Map (Int, Seq Int) Int
        f acc (k, prefix) = case viewr prefix of
                              smolBoys :> bigBoy ->
                                if bigBoy /= k
                                then Map.insert (k, prefix) (acc ! (k, smolBoys)) acc
                                else Map.insert (k, prefix) (sum . map (\z -> acc ! (z, smolBoys)) . filter (>= 0) $ [k - 1, k - 2, k - 3]) acc
                              Seq.EmptyR ->
                                if k == 0 then Map.insert (0, prefix) 1 acc else Map.insert (k, prefix) 0 acc

printResults :: [Int] -> PuzzleAnswerPair
printResults joltages = PuzzleAnswerPair (part1, part2)
  where
    (diffOf1, diffOf3) = joltageDifferences joltages
    part1 = show $ diffOf1 * diffOf3
    part2 = show . validArrangements . sort $ joltages

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 10
