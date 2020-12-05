module Advent2020.Day05
  ( solve
  ) where

import qualified Data.Set as Set
import qualified Data.List as List
import Control.Monad (guard)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char)
import Text.Parsec (many1, (<|>))

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf)

data Seat = Seat { row :: Int, col :: Int } deriving (Show, Eq, Ord)

inputParser :: Parser [Seat]
inputParser = linesOf seat
  where
    asInt = foldl (\acc x -> acc * 2 + x) 0
    seat = (\r c -> Seat { row=r, col=c }) <$> (asInt <$> many1 fb) <*> (asInt <$> many1 lr)
    fb = (0 <$ char 'F') <|> (1 <$ char 'B')
    lr = (0 <$ char 'L') <|> (1 <$ char 'R')

seatId :: Seat -> Int
seatId seat = row seat * 8 + col seat

mySeatId :: [Seat] -> Int
mySeatId seats = head $ do
  let seatIds = map seatId . List.sort $ seats
  (a, b) <- zip seatIds . tail $ seatIds
  guard $ abs (a - b) == 2
  pure $ (a + b) `div` 2

printResults :: [Seat] -> PuzzleAnswerPair
printResults seats = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . seatId . maximum $ seats
    part2 = show . mySeatId $ seats

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 5
