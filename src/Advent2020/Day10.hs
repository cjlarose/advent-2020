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

newtype Joltage = Joltage Int deriving (Eq, Ord)

inputParser :: Parser [Joltage]
inputParser = linesOf (Joltage <$> integer)

joltageDifferences :: [Joltage] -> (Int, Int)
joltageDifferences xs = (diffsOf 1, diffsOf 3 + 1)
  where
    diffsOf k = length . filter (== k) $ diffs
    diffs :: [Int]
    diffs = map (\[Joltage x, Joltage y] -> y - x) . filter (\l -> length l == 2) . subLists $ (Joltage 0 : sort xs)

validArrangements :: [Joltage] -> Int
validArrangements xs = waysToGetTo ! (Joltage desiredJoltage, joltages |> Joltage desiredJoltage)
  where
    joltages = Seq.fromList xs

    desiredJoltage :: Int
    desiredJoltage = case viewr joltages of
                       _ :> Joltage j -> j

    waysToGetTo :: Map (Joltage, Seq Joltage) Int
    waysToGetTo = foldl' f Map.empty $ [(Joltage j, Seq.fromList prefix) |  prefix <- inits (xs ++ [Joltage desiredJoltage]), j <- [0..desiredJoltage]]
      where
        f :: Map (Joltage, Seq Joltage) Int -> (Joltage, Seq Joltage) -> Map (Joltage, Seq Joltage) Int
        f acc (Joltage k, prefix) =
          case viewr prefix of
            smolBoys :> bigBoy ->
              if bigBoy /= Joltage k
              then Map.insert (Joltage k, prefix) (acc ! (Joltage k, smolBoys)) acc
              else Map.insert (Joltage k, prefix) (sum . map (\z -> acc ! (Joltage z, smolBoys)) . filter (>= 0) $ [k - 1, k - 2, k - 3]) acc
            Seq.EmptyR ->
              if k == 0 then Map.insert (Joltage k, prefix) 1 acc else Map.insert (Joltage k, prefix) 0 acc

printResults :: [Joltage] -> PuzzleAnswerPair
printResults joltages = PuzzleAnswerPair (part1, part2)
  where
    (diffOf1, diffOf3) = joltageDifferences joltages
    part1 = show $ diffOf1 * diffOf3
    part2 = show . validArrangements . sort $ joltages

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 10
