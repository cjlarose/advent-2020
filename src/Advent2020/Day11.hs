{-# LANGUAGE TupleSections #-}

module Advent2020.Day11
  ( solve
  ) where

import qualified Data.Set as Set
import Data.Set (Set, member)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Data.Maybe (catMaybes, maybeToList)
import Control.Monad (guard, forM_, foldM)
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed.Mutable as UMV
import Text.Parsec.ByteString (Parser)
import Text.Parsec (many1, (<|>), getPosition, sourceLine, sourceColumn)
import Text.Parsec.Char (char)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf)

data Seat = Empty | Occupied deriving (Show, Eq)
data WaitingArea = WaitingArea { getSeats :: Set (Int, Int)
                               , getOccupiedSeats :: Set (Int, Int)
                               , getM :: Int
                               , getN :: Int } deriving (Eq)
type SeatUpdateRule = WaitingArea -> Set (Int, Int)

instance Show WaitingArea where
  show waitingArea = unlines rows
    where
      rows = map row [0..getN waitingArea - 1]
      row i = map (\j -> toChar . seatAt (i, j) $ waitingArea) [0..getM waitingArea - 1]
      toChar (Just Empty) = 'L'
      toChar (Just Occupied) = '#'
      toChar Nothing = '.'

seatAt :: (Int, Int) -> WaitingArea -> Maybe Seat
seatAt k waitingArea | k `member` getOccupiedSeats waitingArea = Just Occupied
                     | k `member` getSeats waitingArea = Just Empty
                     | otherwise = Nothing

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
    toMap xs = WaitingArea { getSeats = Set.fromList . catMaybes . concat $ xs
                           , getOccupiedSeats = Set.empty
                           , getN = length xs
                           , getM = length (head xs)
                           }

neighbors :: (Int, Int) -> WaitingArea -> [(Int, Int)]
neighbors (i, j) WaitingArea{ getSeats=seats } = do
  ii <- [i-1..i+1]
  jj <- [j-1..j+1]
  guard $ (i, j) /= (ii, jj)
  guard $ (ii, jj) `member` seats
  pure (ii, jj)

firstVisibleSeatInDirection :: (Int, Int) -> (Int, Int) -> WaitingArea -> Maybe (Int, Int)
firstVisibleSeatInDirection (i, j) (di, dj) waitingArea@WaitingArea{getN=n,getM=m} = firstVisible waitingArea coords
  where
    coords = takeWhile inBounds . drop 1 . iterate (\(ii, jj) -> (ii + di, jj + dj)) $ (i, j)
    inBounds (ii, jj) = ii >= 0 && ii < n && jj >= 0 && jj < m

firstVisible :: WaitingArea -> [(Int, Int)] -> Maybe (Int, Int)
firstVisible _ [] = Nothing
firstVisible waitingArea@WaitingArea{getSeats=seats} (x:xs)
  | x `member` seats = Just x
  | otherwise = firstVisible waitingArea xs

visibleSeats :: (Int, Int) -> WaitingArea -> [(Int, Int)]
visibleSeats pos waitingArea = do
  di <- [-1, 0, 1]
  dj <- [-1, 0, 1]
  let dir = (di, dj)
  guard $ dir /= (0, 0)
  maybeToList . firstVisibleSeatInDirection pos dir $ waitingArea

simulateRound :: SeatUpdateRule -> WaitingArea -> WaitingArea
simulateRound f w = w { getOccupiedSeats = f w }

occupied :: WaitingArea -> (Int, Int) -> Bool
occupied waitingArea coord = coord `member` getOccupiedSeats waitingArea

naiveRule :: WaitingArea -> SeatUpdateRule
naiveRule originalW = updateSeat
  where
    neighborsMap :: Map (Int, Int) [(Int, Int)]
    neighborsMap = Map.fromSet (`neighbors` originalW) . getSeats $ originalW


    updateSeat :: WaitingArea -> Set (Int, Int)
    updateSeat w = runST $ do
      occupiedNeighbors <- UMV.replicate (getN w * getM w) (0 :: Int)
      let keyFor (i, j) = i * getN w + j
      let increment coord = UMV.modify occupiedNeighbors (1 +) (keyFor coord)
      forM_ (getOccupiedSeats w) $ \coord -> do
        let neighbors = neighborsMap ! coord
        forM_ neighbors increment
      let g acc coord val | coord `member` getOccupiedSeats w = if val < 4 then Set.insert coord acc else acc
                          | otherwise = if val == 0 then Set.insert coord acc else acc
      foldM (\acc coord -> UMV.read occupiedNeighbors (keyFor coord) >>= (\val -> pure $ g acc coord val)) Set.empty . getSeats $ originalW

realisticRule :: WaitingArea -> SeatUpdateRule
realisticRule originalW = updateSeat
  where
    neighborsMap :: Map (Int, Int) [(Int, Int)]
    neighborsMap = Map.fromSet (`visibleSeats` originalW) . getSeats $ originalW

    updateSeat :: WaitingArea -> Set (Int, Int)
    updateSeat w@WaitingArea{getSeats=seats} = Set.filter isNowOccupied seats
      where
        isNowOccupied :: (Int, Int) -> Bool
        isNowOccupied pos | occupied w pos = (< 5) . length . filter (occupied w) . (neighborsMap !) $ pos
                          | otherwise = not . any (occupied w) . (neighborsMap !) $ pos

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
totalOccupied = Set.size . getOccupiedSeats

printResults :: WaitingArea -> PuzzleAnswerPair
printResults waitingArea = PuzzleAnswerPair (part1, part2)
  where
    part1 = maybe "no stable state" (show . totalOccupied) $ stableState (naiveRule waitingArea) waitingArea
    part2 = maybe "no stable state" (show . totalOccupied) $ stableState (realisticRule waitingArea) waitingArea

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 11
