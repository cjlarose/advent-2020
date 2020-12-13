module Advent2020.Day13
  ( solve
  ) where

import Numeric.Natural (Natural)
import Data.List (find)
import Data.Maybe (isJust)
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

multipleOf :: Natural -> Natural -> Bool
multipleOf x k = x `mod` k == 0

earliestTime :: Natural -> [BusId] -> (Natural, BusId)
earliestTime start busIds = go start
  where
    go t = case find (\(BusId id) -> t `multipleOf` id) busIds of
             Just busId -> (t, busId)
             Nothing -> go (succ t)

-- https://stackoverflow.com/a/35529381/1231384
crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0, 1)
    where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
        where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1

    -- Modular Inverse
    a `inv` m = let (_, i, _) = gcd a m in i `mod` m

    -- Extended Euclidean Algorithm
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
        where (g, s, t) = gcd (b `mod` a) a

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
    congruences = map (\(offset, BusId modulus) -> (2 * fromIntegral modulus - fromIntegral offset, fromIntegral modulus)) xs
    (k, _) = crt congruences

printResults :: (Natural, [(Int, BusId)]) -> PuzzleAnswerPair
printResults (earliestPossibleTime, busIds) = PuzzleAnswerPair (part1, part2)
  where
    (t, BusId id) = earliestTime earliestPossibleTime . map snd $ busIds
    part1 = show $ (t - earliestPossibleTime) * id
    part2 = show . earliestPossibleInSequenceDepartures $ busIds

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 13
