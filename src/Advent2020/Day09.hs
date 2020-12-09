module Advent2020.Day09
  ( solve
  ) where

import Data.List (find, tails)
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

subLists :: [Int] -> [[Int]]
subLists xs = go 0
  where
    go seqLen | seqLen > length xs = []
              | otherwise = (map (take seqLen) . take (length xs - seqLen) . tails $ xs) ++ go (succ seqLen)

constructableFromPreamble :: [Int] -> Int -> Bool
constructableFromPreamble preamble k = isJust . find (\(a, b) -> a + b == k) . pairs $ preamble

invalidEntry :: [Int] -> Maybe Int
invalidEntry ints = go . zip candidates $ prefixes
  where
    candidates = drop 25 ints
    prefixes = map (\i -> take 25 . drop i $ ints) [0..]
    go ((k, preamble):xs) | not . constructableFromPreamble preamble $ k = Just k
                          | otherwise = go xs
    go [] = Nothing

subsequenceSum :: Int -> [Int] -> Maybe Int
subsequenceSum k xs = (\s -> maximum s + minimum s) <$> seq
  where
    seq = find ((== k) . sum) . filter (\s -> length s >= 2) . subLists $ xs

printResults :: [Int] -> PuzzleAnswerPair
printResults ints = PuzzleAnswerPair (part1, part2)
  where
    res = invalidEntry ints
    (part1, part2) =
      case res of
        Nothing -> error "no invalid entry found"
        Just invalid -> (p1, p2)
          where
            p1 = show invalid
            p2 = case subsequenceSum invalid ints of
                      Just val -> show val
                      Nothing -> error "no subsequence found"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 9
