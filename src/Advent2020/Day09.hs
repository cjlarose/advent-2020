module Advent2020.Day09
  ( solve
  ) where

import Data.List (find)
import Text.Parsec.ByteString (Parser)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integer, linesOf)
import Advent.ListUtils (pairs, subLists)

inputParser :: Parser [Int]
inputParser = linesOf integer

constructableFromPreamble :: [Int] -> Int -> Bool
constructableFromPreamble preamble k = any (\(a, b) -> a + b == k) . pairs $ preamble

invalidEntry :: [Int] -> Maybe Int
invalidEntry ints = go . zip candidates $ prefixes
  where
    candidates = drop 25 ints
    prefixes = map (\i -> take 25 . drop i $ ints) [0..]
    go ((k, preamble):xs) | not . constructableFromPreamble preamble $ k = Just k
                          | otherwise = go xs
    go [] = Nothing

subsequenceSum :: Int -> [Int] -> Maybe Int
subsequenceSum k xs = do
  seq <- find ((== k) . sum) . filter (\s -> length s >= 2) . subLists $ xs
  pure $ maximum seq + minimum seq

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
