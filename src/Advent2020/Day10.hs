module Advent2020.Day10
  ( solve
  ) where

import Numeric.Natural (Natural)
import Data.Int (Int64)
import Data.List (sort, foldl')
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Text.Parsec.ByteString (Parser)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integerWithoutLeadingSign, linesOf)
import Advent.ListUtils (consectutivePairs)

newtype Joltage = Joltage Natural deriving (Eq, Ord)

inputParser :: Parser [Joltage]
inputParser = linesOf (Joltage <$> integerWithoutLeadingSign)

joltageDifferences :: [Joltage] -> (Natural, Natural)
joltageDifferences xs = (diffsOf 1, diffsOf 3 + 1)
  where
    diffsOf k = fromIntegral . length . filter (== k) $ diffs
    diffs :: [Natural]
    diffs = map (\(Joltage x, Joltage y) -> y - x) . consectutivePairs $ (Joltage 0 : sort xs)

possibleInputJoltages :: Joltage -> [Joltage]
possibleInputJoltages (Joltage k) = map (Joltage . fromIntegral) . filter (>= 0) $ candidates
  where
    candidates = [toInteger k - 1, toInteger k - 2, toInteger k - 3]

validArrangements :: [Joltage] -> Int64
validArrangements xs = foldl' f Map.empty sorted ! maximum sorted
  where
    sorted = Joltage 0 : sort xs
    f acc j = Map.insert j numWays acc
      where
        numWays = case j of
                    Joltage 0 -> 1
                    Joltage _ -> sum . map (\k -> Map.findWithDefault 0 k acc) . possibleInputJoltages $ j

printResults :: [Joltage] -> PuzzleAnswerPair
printResults joltages = PuzzleAnswerPair (part1, part2)
  where
    (diffOf1, diffOf3) = joltageDifferences joltages
    part1 = show $ diffOf1 * diffOf3
    part2 = show . validArrangements $ joltages

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 10
