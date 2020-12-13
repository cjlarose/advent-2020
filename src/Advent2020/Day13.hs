module Advent2020.Day13
  ( solve
  ) where

import Numeric.Natural (Natural)
import qualified Data.List as List
import Data.List (find)
import Control.Monad (guard)
import Data.Maybe (catMaybes)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, endOfLine)
import Text.Parsec (many1, (<|>), sepBy1, eof)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (natural)

newtype BusId = BusId Natural deriving Show

inputParser :: Parser (Natural, [BusId])
inputParser = (,) <$> natural <* endOfLine <*> busIds <* endOfLine <* eof
  where
    busIds = catMaybes <$> sepBy1 possibleBusId (char ',')
    possibleBusId = (Nothing <$ char 'x') <|> (Just . BusId <$> natural)

multipleOf :: Natural -> Natural -> Bool
multipleOf x k = x `mod` k == 0

earliestTime :: Natural -> [BusId] -> (Natural, BusId)
earliestTime start busIds = go start
  where
    go t = case find (\(BusId id) -> t `multipleOf` id) busIds of
             Just busId -> (t, busId)
             Nothing -> go (succ t)

printResults :: (Natural, [BusId]) -> PuzzleAnswerPair
printResults (earliestPossibleTime, busIds) = PuzzleAnswerPair (part1, part2)
  where
    (t, BusId id) = earliestTime earliestPossibleTime busIds
    part1 = show $ (t - earliestPossibleTime) * id
    part2 = "not implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 13
