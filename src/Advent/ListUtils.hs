module Advent.ListUtils
  ( subsetsOfCardinalityTwo
  , subLists
  , consectutivePairs
  ) where

import Data.List (tails)

subsetsOfCardinalityTwo :: [Int] -> [(Int, Int)]
subsetsOfCardinalityTwo [] = []
subsetsOfCardinalityTwo (x:xs) = map (\y -> (x, y)) xs ++ subsetsOfCardinalityTwo xs

consectutivePairs :: [a] -> [(a, a)]
consectutivePairs xs =  map (\(a:b:_) -> (a, b)) .  take (length xs - 1) . tails $ xs

-- Returns contiguous subsequences of a list
subLists :: [a] -> [[a]]
subLists xs = [] : go 1
  where
    go seqLen | seqLen > length xs = []
              | otherwise = (map (take seqLen) . take (length xs - seqLen + 1) . tails $ xs) ++ go (succ seqLen)
