module Advent2020.Day13
  ( solve
  ) where

import Numeric.Natural (Natural)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Maybe (isJust)
import Control.Monad (foldM)
import Math.NumberTheory.Moduli.Chinese (chinese)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, endOfLine)
import Text.Parsec ((<|>), sepBy1, eof)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (natural)

newtype BusId = BusId Natural deriving Show

inputParser :: Parser (Natural, [(Int, BusId)])
inputParser = (,) <$> natural <* endOfLine <*> busIds <* endOfLine <* eof
  where
    busIds = map (\(i, Just id) -> (i, id)) . filter (isJust . snd) . zip [0..] <$> sepBy1 possibleBusId (char ',')
    possibleBusId = (Nothing <$ char 'x') <|> (Just . BusId <$> natural)

-- | leastMultipleGreaterThanOrEqualTo k n returns the least z such that z >= k
-- and z `mod` n = 0
leastMultipleGreaterThanOrEqualTo :: Integer -> Integer -> Integer
leastMultipleGreaterThanOrEqualTo k n = k + n - (k `mod` n)

earliestTime :: Natural -> [BusId] -> (Natural, BusId)
earliestTime start = minimumBy (comparing fst) . map (\b -> (nextDeparture b, b))
  where
    nextDeparture :: BusId -> Natural
    nextDeparture (BusId id) = fromIntegral . leastMultipleGreaterThanOrEqualTo (fromIntegral start) . fromIntegral $ id

solutionToSystemOfCongruences :: [(Integer, Integer)] -> Maybe (Integer, Integer)
solutionToSystemOfCongruences = foldM chinese (0, 1)

-- 17,x,13,19
-- t is congruent to 0 (mod 17)
--
-- t + 2 is congruent to 0 (mod 13)
-- t is congruent to 11 (mod 13)
--
-- t + 3 is congruent to 0 (mod 19)
-- t is congruent to 16 (mod 19)
earliestPossibleInSequenceDepartures :: [(Int, BusId)] -> Integer
earliestPossibleInSequenceDepartures xs = k
  where
    toCongruence (offset, BusId busId) = (residue, modulus)
      where
        modulus = fromIntegral busId
        residue = (- fromIntegral offset) `mod` modulus
    congruences = map toCongruence xs
    Just (k, _) = solutionToSystemOfCongruences congruences

printResults :: (Natural, [(Int, BusId)]) -> PuzzleAnswerPair
printResults (earliestPossibleTime, busIds) = PuzzleAnswerPair (part1, part2)
  where
    (t, BusId id) = earliestTime earliestPossibleTime . map snd $ busIds
    part1 = show $ (t - earliestPossibleTime) * id
    part2 = show . earliestPossibleInSequenceDepartures $ busIds

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 13
