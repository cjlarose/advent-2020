{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day20
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import Text.Megaparsec ((<|>), lookAhead, eof, some)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad.Combinators (count)

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse, token, symbol)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

type Edge = [Char]
data Edges = Edges { getTop :: Edge
                   , getBottom :: Edge
                   , getLeft :: Edge
                   , getRight :: Edge
                   } deriving (Show, Ord, Eq)
data Tile = Tile { getId :: Integer
                 , getEdges :: Edges } deriving (Show, Ord, Eq)

inputParser :: Parser [Tile]
inputParser = some tile <* eof
  where
    tile = Tile <$> (symbol "Tile" *> decimal) <* symbol ":" <*> edges
    edges = (\(t, b) (l, r) -> Edges t b l r) <$> lookAhead topAndBottom <*> leftAndRight
    topAndBottom = (,) <$> row <* count 8 row <*> row
    leftAndRight = foldr ((\(l, r) (ls, rs) -> (l:ls, r:rs)) . (\xs -> (head xs, last xs))) ([], []) <$> count 10 row
    row = count 10 (token pixel)
    pixel = char '.' <|> char '#'

eachEdge :: Tile -> [Edge]
eachEdge Tile{getEdges=Edges{getTop=top,getBottom=bottom,getLeft=left,getRight=right}} = [top, bottom, left, right]

possibleEdges :: Tile -> Set Edge
possibleEdges = Set.fromList . concatMap (\edge -> [edge, reverse edge]) . eachEdge

corners :: [Tile] -> [Tile]
corners tiles = filter isCorner tiles
  where
    edgeIdsForTile :: Map Tile (Set Edge)
    edgeIdsForTile = Map.fromList . map (\t -> (t, possibleEdges t)) $ tiles
    isCorner t = Set.size (edgeIdsForTile ! t \\ Set.unions (Map.delete t edgeIdsForTile)) == 4

printResults :: [Tile] -> PuzzleAnswerPair
printResults tiles = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . product . map getId . corners $ tiles
    part2 = "not implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 20
