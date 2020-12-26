{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day23
  ( solve
  ) where

import Data.Char (digitToInt)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Text.Megaparsec (some, eof)
import Text.Megaparsec.Char (digitChar)

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse, token)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

newtype Cup = Cup Int deriving (Show, Eq, Ord)
data CupCircle = CupCircle { getFocus :: Cup
                           , getNext :: Map Cup Cup
                           } deriving Show

clockwiseCups :: CupCircle -> [Cup]
clockwiseCups CupCircle{getFocus=focus,getNext=nextMap} = focus : (takeWhile (/= focus) . drop 1 . iterate findNext $ focus)
  where
    findNext cup = nextMap ! cup

setNextCup :: Cup -> Cup -> CupCircle -> CupCircle
setNextCup from to circle@CupCircle{getNext=nextMap} = circle{getNext=Map.insert from to nextMap}

getNextCup :: Cup -> CupCircle -> Cup
getNextCup cup circle = getNext circle ! cup

rotateRight :: CupCircle -> CupCircle
rotateRight circle = let newFocus = getNextCup (getFocus circle) circle
                     in circle{getFocus=newFocus}

cupCirclefromList :: [Cup] -> CupCircle
cupCirclefromList xs = CupCircle focus nextMap
  where
    focus = head xs
    nexts = zip xs (tail xs ++ [head xs])
    nextMap = foldr (\(from, to) acc -> Map.insert from to acc) Map.empty nexts

inputParser :: Parser CupCircle
inputParser = cupCirclefromList <$> some cup <* eof
  where
    cup = Cup . digitToInt <$> token digitChar

simulateRounds :: Int -> CupCircle -> CupCircle
simulateRounds k initialCircle = go k initialCircle
  where
    maxC = maxCup initialCircle
    destCupForCurrent (Cup x) exclude =
      let target = if x == 1 then maxC else Cup (x - 1)
      in if target `Set.member` exclude then destCupForCurrent target exclude else target
    go 0 circle = circle
    go n circle =
      let removed@[a,_,c] = take 3 . drop 1 . clockwiseCups $ circle
          with3Removed = setNextCup (getFocus circle) (getNextCup c circle) circle
          destCup = destCupForCurrent (getFocus circle) . Set.fromList $ removed
          nextOfDest = getNextCup destCup with3Removed
          with3AddedBack = setNextCup c nextOfDest . setNextCup destCup a $ with3Removed
          newCircle = rotateRight with3AddedBack
      in go (n - 1) newCircle

bringCupToFront :: Cup -> CupCircle -> CupCircle
bringCupToFront cup circle = circle{getFocus=cup}

cupsAfterLabel1 :: CupCircle -> String
cupsAfterLabel1 circle = concatMap (show . (\(Cup x) -> x)) rest
  where
    rest = tail . clockwiseCups . bringCupToFront (Cup 1) $ circle

maxCup :: CupCircle -> Cup
maxCup = maximum . clockwiseCups

extendToCupLabelled1Million :: CupCircle -> CupCircle
extendToCupLabelled1Million circle = newCircle
  where
    (Cup maxValue) = maxCup circle
    newCups = map Cup [maxValue + 1..1000000]
    lastCupClockwise = last . clockwiseCups $ circle
    nexts = zip (lastCupClockwise : newCups) (newCups ++ [getFocus circle])
    newCircle = foldr (\(from, to) acc -> setNextCup from to acc) circle nexts

productOfTwoCupsAfter1 :: CupCircle -> Int
productOfTwoCupsAfter1 circle = a * b
  where
    (_ : (Cup a) : (Cup b) : _) = clockwiseCups . bringCupToFront (Cup 1) $ circle

printResults :: CupCircle -> PuzzleAnswerPair
printResults circle = PuzzleAnswerPair (part1, part2)
  where
    part1 = cupsAfterLabel1 . simulateRounds 100 $ circle
    part2 = show . productOfTwoCupsAfter1 . simulateRounds 10000000 . extendToCupLabelled1Million $ circle

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 23
