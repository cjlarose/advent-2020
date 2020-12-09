module Advent2020.Day09
  ( solve
  ) where

import Data.List (find)
import Data.Maybe (isJust)
import Text.Parsec.ByteString (Parser)

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

subsequenceSum :: Int -> [Int] -> Int
subsequenceSum k xs = maximum seq + minimum seq
  where
    seq = go 2
    go seqLen = case find ((== k) . sum) (map (\i -> take seqLen . drop i $ xs) [0..length xs]) of
                  Nothing -> go . succ $ seqLen
                  Just seq -> seq

printResults :: [Int] -> PuzzleAnswerPair
printResults ints = PuzzleAnswerPair (part1, part2)
  where
    invalid = head . map fst . filter (not . snd) $ zipWith (\k prev25 -> (k, constructableFromPreamble prev25 k)) (drop 25 ints) (map (\i -> take 25 . drop i $ ints) [0..])
    part1 = show invalid
    part2 = show . subsequenceSum invalid $ ints

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 9
