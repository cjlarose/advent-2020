module Advent2020.Day05
  ( solve
  ) where

import qualified Data.List as List
import Control.Monad (guard)
import Data.Maybe (listToMaybe)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char)
import Text.Parsec (many1, (<|>))

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf)
import Advent.BitUtils (fromBits)

data Seat = Seat { row :: Int, col :: Int } deriving (Show, Eq, Ord)

inputParser :: Parser [Seat]
inputParser = linesOf seat
  where
    seat = (\r c -> Seat { row=r, col=c }) <$> (fromBits <$> many1 fb) <*> (fromBits <$> many1 lr)
    fb = (False <$ char 'F') <|> (True <$ char 'B')
    lr = (False <$ char 'L') <|> (True <$ char 'R')

seatId :: Seat -> Int
seatId seat = row seat * 8 + col seat

mySeatId :: [Seat] -> Maybe Int
mySeatId seats = listToMaybe $ do
  let seatIds = map seatId . List.sort $ seats
  (a, b) <- zip seatIds . tail $ seatIds
  guard $ b - a == 2
  pure $ a + 1

printResults :: [Seat] -> PuzzleAnswerPair
printResults seats = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . seatId . maximum $ seats
    part2 = case mySeatId seats of
              Nothing -> error "No available seat"
              Just mySeat -> show mySeat

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 5
