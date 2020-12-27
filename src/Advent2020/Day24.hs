{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day24
  ( solve
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Text.Megaparsec (some, eof, try)
import Text.Megaparsec.Char (string)
import Control.Monad.Combinators (choice)

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse, token)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

data Direction = E | SE | SW | W | NW | NE deriving Show
type Path = [Direction]
type Vec = (Int, Int, Int)

toDisplacementVector :: Direction -> Vec
toDisplacementVector E = (1, -1, 0)
toDisplacementVector SE = (0, -1, 1)
toDisplacementVector SW = (-1, 0, 1)
toDisplacementVector W = (-1, 1, 0)
toDisplacementVector NW = (0, 1, -1)
toDisplacementVector NE = (1, 0, -1)

inputParser :: Parser [Path]
inputParser = some path <* eof
  where
    path = token . some $ direction
    direction = choice . map try $ [ dir E "e"
                                   , dir SE "se"
                                   , dir SW "sw"
                                   , dir W "w"
                                   , dir NW "nw"
                                   , dir NE "ne"
                                   ]
    dir f s = f <$ string s

toCoord :: Path -> Vec
toCoord = foldr ((\(di, dj, dk) (i, j, k) -> (i + di, j + dj, k + dk)) . toDisplacementVector) (0, 0, 0)

blackTiles :: [Path] -> Set Vec
blackTiles = foldr (f . toCoord) Set.empty
  where
    f coord acc = if coord `Set.member` acc
                  then Set.delete coord acc
                  else Set.insert coord acc

printResults :: [Path] -> PuzzleAnswerPair
printResults paths = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . Set.size . blackTiles $ paths
    part2 = "not implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 24
