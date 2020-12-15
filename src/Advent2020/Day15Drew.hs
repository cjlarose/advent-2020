{-# LANGUAGE BangPatterns #-}

module Advent2020.Day15Drew
  ( main,
    part1,
    part2,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map

type Acc = (Map Int Int, Int, Int)

input :: [Int]
input = [0, 20, 7, 16, 1, 18, 15]

nextItem :: Acc -> Acc
nextItem (m, i, !prev) =
  let m' = Map.insert prev i m
      next = case Map.lookup prev m of
        Nothing -> 0
        Just j -> i - j
   in (m', i + 1, next)

buildSequence :: [Int] -> [Int]
buildSequence start =
  let m = Map.fromList $ zip (init start) [0 ..]
      rest = drop 1 . map (\(_, _, x) -> x) . iterate nextItem $ (m, length start - 1, last start)
   in start <> rest

part1 :: [Int] -> Int
part1 start = buildSequence start !! 2019

part2 :: [Int] -> Int
part2 start = buildSequence start !! 29999999

main :: IO ()
main = do
  print $ part1 input
  print $ part2 input
