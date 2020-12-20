{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day20
  ( solve
  ) where

import Debug.Trace (traceShow)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import Control.Monad (guard)
import Control.Monad.Loops (iterateUntilM)
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
type TileId = Integer
data Tile = Tile { getId :: TileId
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

rotateRight :: Tile -> Tile
rotateRight t@Tile{getEdges=Edges{getTop=top,getBottom=bottom,getLeft=left,getRight=right}}
  = t{getEdges=Edges newTop newBottom newLeft newRight}
  where
    newTop = reverse left
    newBottom = reverse right
    newLeft = bottom
    newRight = top

flipV :: Tile -> Tile
flipV t@Tile{getEdges=Edges{getTop=top,getBottom=bottom,getLeft=left,getRight=right}}
  = t{getEdges=Edges newTop newBottom newLeft newRight}
  where
    newTop = bottom
    newBottom = top
    newLeft = reverse left
    newRight = reverse right

topEdge = getTop . getEdges
leftEdge = getLeft . getEdges
rightEdge = getRight . getEdges
bottomEdge = getBottom . getEdges

eqivalanceClass :: Tile -> [Tile]
eqivalanceClass t = rotationsOf t ++ rotationsOf (flipV t)
  where
    rotationsOf = take 4 . iterate rotateRight

implies :: Bool -> Bool -> Bool
implies p q = not p || q

globallyDistinctEdges :: [Tile] -> Set Edge
globallyDistinctEdges tiles = Set.unions distinctEdgePairs
  where
    tileEdges :: [(Set Edge, Tile)]
    tileEdges = do
      tile <- tiles
      edge <- eachEdge tile
      let edgePair = Set.fromList [edge, reverse edge]
      pure (edgePair, tile)
    edgePairsByTileId :: Map (Set Edge) (Set TileId)
    edgePairsByTileId = foldr (\(edgePair, tile) -> Map.insertWith Set.union edgePair (Set.singleton . getId $ tile)) Map.empty tileEdges
    distinctEdgePairs = Map.keys . Map.filter ((== 1) . Set.size) $ edgePairsByTileId

jigsaw :: [Tile] -> [Map (Integer, Integer) Tile]
jigsaw tiles = completedPuzzles
  where
    distinctEdges = globallyDistinctEdges tiles
    globallyDistinctEdge e = Set.member e distinctEdges
    everyTileInEveryOrientation = concatMap eqivalanceClass tiles
    searchStart = (Map.empty, Set.singleton (0, 0), Set.empty)
    isComplete (m, _, _) = Map.size m == length tiles
    selectPiece (puzzle, queue, visited) = do
      let coord@(i,j) = Set.findMin queue
      -- tileIds isn't a member of the visited set
      -- if not in the first column, left edge must match right edge of left neighbor
      -- if not in the first row, top edge must match bottom edge of top neighbor
      -- if in top row, top edge must be distinct
      -- if in bottom row, bottom edge must be distinct
      -- if in left column, left edge must be distinct
      -- if in right column, right edge must be distinct
      let candidates = do tile <- everyTileInEveryOrientation
                          guard . not . Set.member (getId tile) $ visited
                          -- let leftNeighbor = puzzle ! (traceShow ("leftNeighbor", (i, j - 1)) (i, j - 1))
                          let leftNeighbor = puzzle ! (i, j - 1)
                          guard . implies (j > 0) . (leftEdge tile == ) . rightEdge $ leftNeighbor
                          -- let topNeighbor = puzzle ! (traceShow ("topNeighbor", (i - 1, j)) (i - 1, j))
                          let topNeighbor = puzzle ! (i - 1, j)
                          guard . implies (i > 0) . (topEdge tile == ) . bottomEdge $ topNeighbor
                          guard . implies (i == 0) . globallyDistinctEdge . topEdge $ tile
                          guard . implies (i == 11) . globallyDistinctEdge . bottomEdge $ tile
                          guard . implies (j == 0) . globallyDistinctEdge . leftEdge $ tile
                          guard . implies (j == 11) . globallyDistinctEdge . rightEdge $ tile
                          pure tile
      -- candidate <- traceShow ("candidates for", coord, length candidates, candidates) (traceShow ("coord", coord) candidates)
      candidate <- candidates
      let neighbors = do (i, j) <- [(i + 1, j), (i, j + 1)]
                         guard $ i < 12 && j < 12
                         pure (i, j)
      pure ( Map.insert coord candidate puzzle
           , foldr Set.insert (Set.delete coord queue) neighbors
           , Set.insert (getId candidate) visited )
    completedPuzzles = map (\(m, _, _) -> m) . iterateUntilM isComplete selectPiece $ searchStart

printResults :: [Tile] -> PuzzleAnswerPair
printResults tiles = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . product . map getId . corners $ tiles
    part2 = show . length . jigsaw $ tiles

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 20
