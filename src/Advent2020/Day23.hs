{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day23
  ( solve
  ) where

import qualified Data.List as List
import Data.Char (digitToInt)
import Data.Sequence (Seq((:<|)), (<|), (|>), (><))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Foldable (toList)
import Text.Megaparsec (some, eof)
import Text.Megaparsec.Char (digitChar)

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse, token)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

newtype Cup = Cup Int deriving (Show, Eq, Ord)
type CupCircle = Seq Cup

inputParser :: Parser CupCircle
inputParser = Seq.fromList <$> some cup <* eof
  where
    cup = Cup . digitToInt <$> token digitChar

simulateRounds :: Int -> CupCircle -> CupCircle
simulateRounds k initialCircle = go k initialCircle
  where
    sortedDescCups = reverse . List.sort . toList $ initialCircle
    destCupForCurrent x exclude =
      let candidates = filter (`Set.notMember` exclude) sortedDescCups
      in head $ dropWhile (>= x) candidates ++ candidates
    rotateLeft (front :<| rest) = rest |> front
    go 0 circle = circle
    go n (current :<| rest) =
      let (removed, afterRemoved) = Seq.splitAt 3 rest
          with3Removed = current <| afterRemoved
          destCup = destCupForCurrent current (Set.fromList . toList $ removed)
          (beforeDest, dest :<| afterDest) = Seq.spanl (destCup /=) with3Removed
          with3AddedBack = (beforeDest |> dest) >< removed >< afterDest
          newCircle = rotateLeft with3AddedBack
      in go (n - 1) newCircle

bringCupToFront :: Cup -> CupCircle -> CupCircle
bringCupToFront cup circle = after >< before
  where
    (before, after) = Seq.spanl (cup /=) circle

cupsAfterLabel1 :: CupCircle -> String
cupsAfterLabel1 circle = concatMap (show . (\(Cup x) -> x)) . toList $ rest
  where
    (_ :<| rest) = bringCupToFront (Cup 1) circle

printResults :: CupCircle -> PuzzleAnswerPair
printResults circle = PuzzleAnswerPair (part1, part2)
  where
    part1 = cupsAfterLabel1 . simulateRounds 100 $ circle
    part2 = "not implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 23
