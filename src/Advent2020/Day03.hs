module Advent2020.Day03
  ( solve
  ) where

import Data.Maybe (Maybe(..), isJust, fromJust)
import Text.Parsec.ByteString (Parser)
import Text.Parsec (many1, (<|>))
import Text.Parsec.Char (string, lower, char)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf, nonNegativeInteger)

data MapSquare = OpenSquare | Tree deriving Show
type MapLine = [MapSquare]
data Slope = Slope { di :: Int, dj :: Int }

mapLines :: Parser [MapLine]
mapLines = linesOf mapLine
  where
    mapLine :: Parser MapLine
    mapLine = cycle <$> many1 mapSquare

    mapSquare :: Parser MapSquare
    mapSquare = (OpenSquare <$ char '.') <|> (Tree <$ char '#')

squaresAtSlope :: Slope -> [MapLine] -> [MapSquare]
squaresAtSlope (Slope di dj) lines = map fromJust . takeWhile isJust . map squareAt $ coords
  where
    squareAt (i, j) | i >= length lines = Nothing
                    | otherwise = Just $ lines !! i !! j

    coords = iterate (\(i, j) -> (i + di, j + dj)) (0, 0)

treesOnSlope :: Slope -> [MapLine] -> Int
treesOnSlope slope = length . filter isTree . squaresAtSlope slope
  where
    isTree Tree = True
    isTree _ = False

printResults :: [MapLine] -> PuzzleAnswerPair
printResults lines = PuzzleAnswerPair (part1, part2)
  where
    part1 = show $ treesOnSlope (Slope 1 3) lines
    slopes = [Slope 1 1, Slope 1 3, Slope 1 5, Slope 1 7, Slope 2 1]
    part2 = show . product . map (`treesOnSlope` lines) $ slopes

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse mapLines printResults <$> getProblemInputAsByteString 3
