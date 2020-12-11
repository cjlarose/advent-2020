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
data WaitingArea = WaitingArea { getSeats :: Map (Int, Int) Seat
                               , getM :: Int
                               , getN :: Int } deriving (Show, Eq)
type SeatUpdateRule = WaitingArea -> (Int, Int) -> Seat -> Seat

inputParser :: Parser WaitingArea
inputParser = toMap <$> linesOf row
  where
    row = many1 seat
    seat = (Nothing <$ char '.') <|> (recordPos <* char 'L')
    recordPos = do
      pos <- getPosition
      let line = sourceLine pos
      let col = sourceColumn pos
      pure . Just $ (line - 1, col - 1)
    toMap xs = WaitingArea { getSeats = Map.fromList . map (, Empty) . catMaybes . concat $ xs
                           , getN = length xs
                           , getM = length (head xs)
                           }

neighbors :: (Int, Int) -> WaitingArea -> [Seat]
neighbors (i, j) WaitingArea{ getSeats=seats } = do
  ii <- [i-1..i+1]
  jj <- [j-1..j+1]
  guard $ (i, j) /= (ii, jj)
  maybeToList . Map.lookup (ii, jj) $ seats

simulateRound :: SeatUpdateRule -> WaitingArea -> WaitingArea
simulateRound f w@WaitingArea{getSeats=seats} = w { getSeats = Map.mapWithKey (f w) seats }

empty :: Seat -> Bool
empty = (==) Empty

occupied :: Seat -> Bool
occupied = (==) Occupied

naiveRule :: SeatUpdateRule
naiveRule w pos Empty | all empty . neighbors pos $ w = Occupied
                      | otherwise = Empty
naiveRule w pos Occupied | (>= 4) . length . filter occupied . neighbors pos $ w = Empty
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
totalOccupied WaitingArea{getSeats=seats} = Map.foldl f 0 seats
  where
    f acc Occupied = acc + 1
    f acc _ = acc

printResults :: WaitingArea -> PuzzleAnswerPair
printResults waitingArea = PuzzleAnswerPair (part1, part2)
  where
    part1 = maybe "no stable state" (show . totalOccupied) $ stableState naiveRule waitingArea
    part2 = "not yet implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 11
