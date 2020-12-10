module Advent2020.Day10
  ( solve
  ) where

import Data.Int (Int64)
import Data.List (sort, foldl', inits)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, viewr, ViewR((:>)))
import qualified Data.Sequence as Seq
import Text.Parsec.ByteString (Parser)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integer, linesOf)
import Advent.ListUtils (consectutivePairs)

newtype Joltage = Joltage Int deriving (Eq, Ord)

inputParser :: Parser [Joltage]
inputParser = linesOf (Joltage <$> integer)

joltageDifferences :: [Joltage] -> (Int, Int)
joltageDifferences xs = (diffsOf 1, diffsOf 3 + 1)
  where
    diffsOf k = length . filter (== k) $ diffs
    diffs :: [Int]
    diffs = map (\(Joltage x, Joltage y) -> y - x) . consectutivePairs $ (Joltage 0 : sort xs)

possibleInputJoltages :: Joltage -> [Joltage]
possibleInputJoltages (Joltage k) = map Joltage . filter (>= 0) $ [k - 1, k - 2, k - 3]

validArrangements :: [Joltage] -> Int64
validArrangements xs = waysToGetTo ! (Joltage maxJoltage, Seq.length joltages)
  where
    sorted = sort xs
    joltages = Seq.fromList sorted

    maxJoltage :: Int
    maxJoltage = case viewr joltages of
                   _ :> Joltage j -> j

    waysToGetTo :: Map (Joltage, Int) Int64
    waysToGetTo = foldl' f Map.empty $ [(Joltage j, Seq.fromList prefix) |  prefix <- inits sorted, j <- [0..maxJoltage]]
      where
        f :: Map (Joltage, Int) Int64 -> (Joltage, Seq Joltage) -> Map (Joltage, Int) Int64
        f acc (j, prefix) =
          case viewr prefix of
            smolBoys :> bigBoy ->
              if bigBoy /= j
              then Map.insert (j, Seq.length prefix) (acc ! (j, Seq.length smolBoys)) acc
              else Map.insert (j, Seq.length prefix) (sum . map (\z -> acc ! (z, Seq.length smolBoys)) . possibleInputJoltages $ j) acc
            Seq.EmptyR ->
              case j of
                Joltage 0 -> Map.insert (j, Seq.length prefix) 1 acc
                Joltage _ -> Map.insert (j, Seq.length prefix) 0 acc

printResults :: [Joltage] -> PuzzleAnswerPair
printResults joltages = PuzzleAnswerPair (part1, part2)
  where
    (diffOf1, diffOf3) = joltageDifferences joltages
    part1 = show $ diffOf1 * diffOf3
    part2 = show . validArrangements $ joltages

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 10
