{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day03
  ( solve
  ) where

import Text.Megaparsec ((<|>), some, eof)
import Text.Megaparsec.Char (char)

import Advent.Input (getProblemInputAsText)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.Parse (Parser, parse, linesOf)

data MapSquare = OpenSquare | Tree deriving Show
type MapLine = [MapSquare]
type TreeMap = [MapLine]
data Slope = Slope { dj :: Int, di :: Int }

treeMap :: Parser TreeMap
treeMap = linesOf mapLine <* eof
  where
    mapLine = cycle <$> some mapSquare
    mapSquare = (OpenSquare <$ char '.') <|> (Tree <$ char '#')

numTreesOnSlope :: Slope -> TreeMap -> Int
numTreesOnSlope Slope { di=di, dj=dj } = numTrees 0 0
  where
    numTrees j acc m@(line:_) = numTrees (j + dj) newCount $ drop di m
      where newCount = case line !! j of
                         Tree -> 1 + acc
                         OpenSquare -> acc
    numTrees _ acc _ = acc

printResults :: TreeMap -> PuzzleAnswerPair
printResults lines = PuzzleAnswerPair (part1, part2)
  where
    part1 = show $ numTreesOnSlope (Slope 3 1) lines
    slopes = [Slope 1 1, Slope 3 1, Slope 5 1, Slope 7 1, Slope 1 2]
    part2 = show . product . map (`numTreesOnSlope` lines) $ slopes

solve :: IO (Either String PuzzleAnswerPair)
solve = parse treeMap printResults <$> getProblemInputAsText 3
