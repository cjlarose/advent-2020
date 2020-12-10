module Advent.ListUtils
  ( pairs
  , subLists
  ) where

import Data.List (tails)

-- Returns all pairs of members of a list
pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs (x:xs) = map (\y -> (x, y)) xs ++ pairs xs

-- Returns contiguous subsequences of a list
subLists :: [a] -> [[a]]
subLists xs = [] : go 1
  where
    go seqLen | seqLen > length xs = []
              | otherwise = (map (take seqLen) . take (length xs - seqLen + 1) . tails $ xs) ++ go (succ seqLen)
