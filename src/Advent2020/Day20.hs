{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day20
  ( solve
  ) where

import Debug.Trace (traceShow)
import Data.List (intersperse)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import Data.Bits ((.&.), shiftL, shiftR, popCount, testBit)
import Control.Monad (guard, forM_)
import Control.Monad.Loops (iterateUntilM)
import Text.Megaparsec ((<|>), lookAhead, eof, some, parse)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad.Combinators (count)

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, token, symbol)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.BitUtils (fromBits)

type Edge = [Char]
data Edges = Edges { getTop :: Edge
                   , getBottom :: Edge
                   , getLeft :: Edge
                   , getRight :: Edge
                   } deriving (Show, Ord, Eq)
type TileId = Integer
data Tile = Tile { getId :: TileId
                 , getEdges :: Edges
                 , getCenter :: [String] } deriving (Show, Ord, Eq)
type Jigsaw = Map (Int, Int) Tile
data Image = Image { getPixels :: [Integer]
                   , getWidth :: Int
                   , getHeight :: Int
                   }

inputParser :: Parser [Tile]
inputParser = some tile <* eof
  where
    tile = Tile <$> (symbol "Tile" *> decimal) <* symbol ":" <*> lookAhead edges <*> center
    edges = (\(t, b) (l, r) -> Edges t b l r) <$> lookAhead topAndBottom <*> leftAndRight
    topAndBottom = (,) <$> row <* count 8 row <*> row
    leftAndRight = foldr ((\(l, r) (ls, rs) -> (l:ls, r:rs)) . (\xs -> (head xs, last xs))) ([], []) <$> count 10 row
    center = row *> count 8 (take 8 . drop 1 <$> row) <* row
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
rotateRight t@Tile{getEdges=Edges{getTop=top,getBottom=bottom,getLeft=left,getRight=right},getCenter=center}
  = t{getEdges=Edges newTop newBottom newLeft newRight,getCenter=newCenter}
  where
    newTop = reverse left
    newBottom = reverse right
    newLeft = bottom
    newRight = top
    n = length center
    m = length . head $ center
    newCenter = map (\i -> map (\j -> center !! (n - 1 - j) !! i) [0..m-1]) [0..n-1]

-- (0, 0) ... (0, m - 1)
-- .
-- .
-- .
-- (n - 1, 0) .... (n - 1, m - 1)
-- 
-- to:
-- (n - 1, 0) ... (0, 0)
-- .
-- .
-- .
-- (n - 1, m - 1) .... (0, m -1)

flipV :: Tile -> Tile
flipV t@Tile{getEdges=Edges{getTop=top,getBottom=bottom,getLeft=left,getRight=right},getCenter=center}
  = t{getEdges=Edges newTop newBottom newLeft newRight,getCenter=newCenter}
  where
    newTop = bottom
    newBottom = top
    newLeft = reverse left
    newRight = reverse right
    n = length center
    m = length . head $ center
    newCenter = map (\i -> map (\j -> center !! (n - 1 - i) !! j) [0..m-1]) [0..n-1]

-- (0, 0) ... (0, m - 1)
-- .
-- .
-- .
-- (n - 1, 0) .... (n - 1, m - 1)
-- 
-- to:
-- (n - 1, 0) .... (n - 1, m - 1)
-- .
-- .
-- .
-- (0, 0) ... (0, m - 1)

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

jigsaw :: [Tile] -> [Jigsaw]
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

extractImage :: Jigsaw -> Image
extractImage puzzle = Image pixels width (traceShow ("tiles in row", map getId . tilesInRow $ 0, "grid", gridForTileRow 0) height)
  where
    tilesInRow i = map (\j -> puzzle ! (i, j)) [0..11]
    toBits = map (== '#')
    gridForTileRow i = foldr (zipWith (++) . getCenter) (replicate 8 "") . tilesInRow $ i
    grid = concatMap gridForTileRow [0..11]
    pixels = map (fromBits . toBits) grid
    width = height
    height = length pixels

allCrops :: Int -> Int -> Image -> [Image]
allCrops w h source = do
  i <- [0..getHeight source - h]
  j <- [0..getWidth source - w]
  let rows = take h . drop i . getPixels $ traceShow ("source w", getWidth source, "source h", getHeight source) source
  let mask = (1 `shiftL` w) - 1
  let cropPixels = map (\row -> (row `shiftR` fromIntegral (getWidth source - j - w)) .&. mask) rows
  pure $ Image cropPixels w h

imageContainsMonster :: Image -> Bool
imageContainsMonster source = and $ zipWith hasMatchInRow (getPixels source) (getPixels monsterImage)
  where
    hasMatchInRow :: Integer -> Integer -> Bool
    hasMatchInRow a b = a .&. b == b

numSeaMonsters :: Image -> Int
numSeaMonsters image = length . filter imageContainsMonster $ traceShow ("length crops", length crops) crops
  where
    crops = allCrops w h image
    Image{getWidth=w,getHeight=h} = monsterImage -- traceShow ("num crops", length (allCrops w h image)) monsterImage

numBlackPixels :: Image -> Int
numBlackPixels = sum . map popCount . getPixels

monsterImage :: Image
monsterImage = Image pixels width height
  where
    pixels = map (fromBits . toBits) tiles
    toBits = map (== '#')
    width = length . head $ tiles
    height = length tiles
    tiles = [ "..................#."
            , "#....##....##....###"
            , ".#..#..#..#..#..#..." ]

showImage :: Image -> String
showImage Image{getPixels=pixels,getWidth=w} = unlines rows
  where
    rows = map showRow pixels
    showRow x = map (\j -> if testBit x (w - 1 - j) then '#' else '.') [0..w-1]

showJigsaw :: Jigsaw -> String
showJigsaw puzzle = unlines rows
  where
    rows = map showRow [0..11]
    tilesInRow i = map (\j -> puzzle ! (i, j)) [0..11]
    showRow = unwords . map (show . getId) . tilesInRow

waterRoughness :: [Tile] -> Int
waterRoughness tiles = traceShow ( "black pixels", numBlackPixels (head images)
                                 , "total monsters", totalMonsters
                                 , "monsters in each image", map numSeaMonsters images
                                 , "black pixels in monster", numBlackPixels monsterImage) roughness
  where
    solutions = jigsaw tiles
    images = map extractImage solutions
    totalMonsters = sum . map numSeaMonsters $ images
    roughness = numBlackPixels (head images) - (totalMonsters * numBlackPixels monsterImage)

printResults :: [Tile] -> PuzzleAnswerPair
printResults tiles = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . product . map getId . corners $ tiles
    part2 = show . waterRoughness $ tiles

solve :: IO (Either String PuzzleAnswerPair)
-- solve = parse inputParser printResults <$> getProblemInputAsText 20

solve = do
  input <- getProblemInputAsText 20
  case parse inputParser "" input of
    Right tiles -> do
      forM_ (jigsaw tiles) $ \puzzle -> do
        putStrLn . showImage . extractImage $ puzzle
        putStrLn . showJigsaw $ puzzle
      -- forM_ (allCrops (getWidth monsterImage) (getHeight monsterImage) (extractImage . head . jigsaw $ tiles)) (putStrLn . showImage)
      -- putStrLn . unlines . getCenter . (\j -> j ! (0, 0)) . head . jigsaw $ tiles
      -- forM_ (jigsaw tiles) (putStrLn . showJigsaw)
      let part1 = show . product . map getId . corners $ tiles
      let part2 = show . waterRoughness $ tiles
      pure . Right . PuzzleAnswerPair $ (part1, part2)
