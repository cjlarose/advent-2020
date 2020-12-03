module Advent2020.Day03
  ( solve
  ) where

import Text.Parsec.ByteString (Parser)
import Text.Parsec (many1, (<|>))
import Text.Parsec.Char (char)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf)

data MapSquare = OpenSquare | Tree deriving Show
type MapLine = [MapSquare]
type TreeMap = [MapLine]
data Slope = Slope { dj :: Int, di :: Int }

treeMap :: Parser TreeMap
treeMap = linesOf mapLine
  where
    mapLine = cycle <$> many1 mapSquare
    mapSquare = (OpenSquare <$ char '.') <|> (Tree <$ char '#')

squaresAtSlope :: Slope -> TreeMap -> [MapSquare]
squaresAtSlope Slope { di=di, dj=dj } lines = squares (0, 0)
  where
    squareAt (i, j) = case drop i lines of
                        line:_ -> Just $ line !! j
                        _ -> Nothing
    squares coord@(i, j) = case squareAt coord of
                             Just square -> square : squares (i + di, j + dj)
                             Nothing -> []

numTreesOnSlope :: Slope -> TreeMap -> Int
numTreesOnSlope slope = length . filter isTree . squaresAtSlope slope
  where
    isTree Tree = True
    isTree _ = False

printResults :: TreeMap -> PuzzleAnswerPair
printResults lines = PuzzleAnswerPair (part1, part2)
  where
    part1 = show $ numTreesOnSlope (Slope 3 1) lines
    slopes = [Slope 1 1, Slope 3 1, Slope 5 1, Slope 7 1, Slope 1 2]
    part2 = show . product . map (`numTreesOnSlope` lines) $ slopes

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse treeMap printResults <$> getProblemInputAsByteString 3
