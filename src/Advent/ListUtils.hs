module Advent.ListUtils
  ( pairs
  , subLists
  ) where

import Data.List (tails)

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs (x:xs) = map (\y -> (x, y)) xs ++ pairs xs

subLists :: [Int] -> [[Int]]
subLists xs = go 0
  where
    go seqLen | seqLen > length xs = []
              | otherwise = (map (take seqLen) . take (length xs - seqLen) . tails $ xs) ++ go (succ seqLen)
