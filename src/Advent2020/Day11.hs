{-# LANGUAGE TupleSections #-}

module Advent2020.Day11
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, maybeToList)
import Control.Monad (guard)
import Text.Parsec.ByteString (Parser)
import Text.Parsec (many1, (<|>), getPosition, sourceLine, sourceColumn)
import Text.Parsec.Char (char)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf)

data Seat = Empty | Occupied deriving (Show, Eq)
newtype WaitingArea = WaitingArea (Map (Int, Int) Seat) deriving (Show, Eq)
type SeatUpdateRule = WaitingArea -> (Int, Int) -> Seat -> Seat

inputParser :: Parser WaitingArea
inputParser = toMap . concat <$> linesOf row
  where
    row = catMaybes <$> many1 seat
    seat = (Nothing <$ char '.') <|> (recordPos <* char 'L')
    recordPos = do
      pos <- getPosition
      let line = sourceLine pos
      let col = sourceColumn pos
      pure . Just $ (line - 1, col - 1)
    toMap = WaitingArea . Map.fromList . map (, Empty)

neighbors :: (Int, Int) -> WaitingArea -> [Seat]
neighbors (i, j) (WaitingArea w) = do
  ii <- [i-1..i+1]
  jj <- [j-1..j+1]
  guard $ (i, j) /= (ii, jj)
  maybeToList . Map.lookup (ii, jj) $ w

simulateRound :: SeatUpdateRule -> WaitingArea -> WaitingArea
simulateRound f w@(WaitingArea waitingArea) = WaitingArea . Map.mapWithKey (f w) $ waitingArea

empty :: Seat -> Bool
empty = (==) Empty

occupied :: Seat -> Bool
occupied = (==) Occupied

newState :: SeatUpdateRule
newState w pos Empty | all empty . neighbors pos $ w = Occupied
                     | otherwise = Empty
newState w pos Occupied | (>= 4) . length . filter occupied . neighbors pos $ w = Empty
                        | otherwise = Occupied

firstRepeatedValue :: Eq a => [a] -> Maybe a
firstRepeatedValue [] = Nothing
firstRepeatedValue (x:xs) = go x xs
  where
    go _ [] = Nothing
    go last (y:ys) | last == y = Just last
                   | otherwise = go y ys

stableState :: SeatUpdateRule -> WaitingArea -> Maybe WaitingArea
stableState f = firstRepeatedValue . iterate (simulateRound f)

totalOccupied :: WaitingArea -> Int
totalOccupied (WaitingArea w) = Map.foldl f 0 w
  where
    f acc Occupied = acc + 1
    f acc _ = acc

printResults :: WaitingArea -> PuzzleAnswerPair
printResults waitingArea = PuzzleAnswerPair (part1, part2)
  where
    part1 = maybe "no stable state" (show . totalOccupied) $ stableState newState waitingArea
    part2 = "not yet implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 11
