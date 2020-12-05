module Advent2020.Day05
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char)
import Text.Parsec (many1, (<|>))

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf, nonNegativeInteger)

newtype Seat = Seat (Int, Int) deriving (Show, Eq, Ord)

inputParser :: Parser [Seat]
inputParser = linesOf seat
  where
    asInt = foldl (\acc x -> acc * 2 + x) 0
    seat = curry Seat <$> (asInt <$> many1 fb) <*> (asInt <$> many1 lr)
    fb = (0 <$ char 'F') <|> (1 <$ char 'B')
    lr = (0 <$ char 'L') <|> (1 <$ char 'R')

pairs [] = []
pairs (x:xs) = map (\y -> (x, y)) xs ++ pairs xs

seatId :: Seat -> Int
seatId (Seat (r, c)) = r * 8 + c

mySeatId :: [Seat] -> Int
mySeatId seats = Set.findMin . Set.difference validSeatIds $ takenSeatIds
  where
    validSeatIds = Set.fromList . map (\(a, b) -> (seatId a + seatId b) `div` 2) . filter (\(a, b) -> abs (seatId a - seatId b) == 2) . pairs $ seats
    takenSeatIds = Set.fromList . map seatId $ seats

printResults :: [Seat] -> PuzzleAnswerPair
printResults seats = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . maximum . map seatId $ seats
    part2 = show . mySeatId $ seats

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 5
