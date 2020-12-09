module Advent2020.Day09
  ( solve
  ) where

import Data.List (find)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (!?), adjust')
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (string, space)
import Text.Parsec ((<|>))

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integer, linesOf)

inputParser :: Parser [Int]
inputParser = linesOf integer

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs (x:xs) = map (\y -> (x, y)) xs ++ pairs xs

constructableFromPreamble :: [Int] -> Int -> Bool
constructableFromPreamble preamble k = isJust . find (\(a, b) -> a + b == k) . pairs $ preamble

printResults :: [Int] -> PuzzleAnswerPair
printResults ints = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . head . filter (not . snd) $ zipWith (\k prev25 -> (k, constructableFromPreamble prev25 k)) (drop 25 ints) (map (\i -> take 25 . drop i $ ints) [0..])
    part2 = "not implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 9
