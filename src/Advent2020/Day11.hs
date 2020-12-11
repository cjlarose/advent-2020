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

firstVisibleSeatInDirection :: (Int, Int) -> (Int, Int) -> WaitingArea -> Maybe Seat
firstVisibleSeatInDirection (i, j) (di, dj) WaitingArea{getSeats=seats,getN=n,getM=m} = firstVisible coords
  where
    coords = takeWhile inBounds . drop 1 . iterate (\(ii, jj) -> (ii + di, jj + dj)) $ (i, j)
    inBounds (ii, jj) = ii >= 0 && ii < n && jj >= 0 && jj < m
    firstVisible :: [(Int, Int)] -> Maybe Seat
    firstVisible [] = Nothing
    firstVisible (x:xs) = case Map.lookup x seats of
                            Just seat -> Just seat
                            Nothing -> firstVisible xs

visibleSeats :: (Int, Int) -> WaitingArea -> [Seat]
visibleSeats pos waitingArea = do
  di <- [-1, 0, 1]
  dj <- [-1, 0, 1]
  let dir = (di, dj)
  guard $ dir /= (0, 0)
  maybeToList . firstVisibleSeatInDirection pos dir $ waitingArea

simulateRound :: SeatUpdateRule -> WaitingArea -> WaitingArea
simulateRound f w@WaitingArea{getSeats=seats} = w { getSeats = Map.mapWithKey (f w) seats }

occupied :: Seat -> Bool
occupied = (==) Occupied

naiveRule :: SeatUpdateRule
naiveRule w pos Empty | not . any occupied . neighbors pos $ w = Occupied
                      | otherwise = Empty
naiveRule w pos Occupied | (>= 4) . length . filter occupied . neighbors pos $ w = Empty
                         | otherwise = Occupied

realisticRule :: SeatUpdateRule
realisticRule w pos Empty | not . any occupied . visibleSeats pos $ w = Occupied
                          | otherwise = Empty
realisticRule w pos Occupied | (>= 5) . length . filter occupied . visibleSeats pos $ w = Empty
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
    part2 = maybe "no stable state" (show . totalOccupied) $ stableState realisticRule waitingArea

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 11
