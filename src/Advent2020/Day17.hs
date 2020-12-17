module Advent2020.Day17
  ( solve
  ) where

import qualified Data.Set as Set
import Data.Set (Set, (\\))
import Data.Maybe (catMaybes)
import Control.Monad (guard)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char)
import Text.Parsec (many1, getPosition, sourceLine, sourceColumn, (<|>))

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf)

type Coord2D = (Int, Int)
type Pocket2Dimension = Set Coord2D

newtype Coord3D = Coord3D (Int, Int, Int) deriving (Eq, Ord)
newtype Coord4D = Coord4D (Int, Int, Int, Int) deriving (Eq, Ord)

class Ord a => HigherDimensionCoord a where
  neighbors :: a -> Set a
  from2d :: Coord2D -> a

instance HigherDimensionCoord Coord3D where
  neighbors coord@(Coord3D (x, y, z)) = Set.fromList $ do
    newX <- [x-1..x+1]
    newY <- [y-1..y+1]
    newZ <- [z-1..z+1]
    let neighbor = Coord3D (newX, newY, newZ)
    guard $ neighbor /= coord
    pure neighbor
  from2d (x, y) = Coord3D (x, y, 0)

instance HigherDimensionCoord Coord4D where
  neighbors coord@(Coord4D (x, y, z, w)) = Set.fromList $ do
    newX <- [x-1..x+1]
    newY <- [y-1..y+1]
    newZ <- [z-1..z+1]
    newW <- [w-1..w+1]
    let neighbor = Coord4D (newX, newY, newZ, newW)
    guard $ neighbor /= coord
    pure neighbor
  from2d (x, y) = Coord4D (x, y, 0, 0)

inputParser :: Parser Pocket2Dimension
inputParser = toSet <$> linesOf row
  where
    row = many1 seat
    seat = (Nothing <$ char '.') <|> (recordPos <* char '#')
    recordPos = do
      pos <- getPosition
      let line = sourceLine pos
      let col = sourceColumn pos
      pure . Just $ (line - 1, col - 1)
    toSet xs = Set.fromList . map (\(i, j) -> (j, i)) . catMaybes . concat $ xs

simulateCycles :: HigherDimensionCoord a => Int -> Set a -> Set a
simulateCycles 0 active = active
simulateCycles rounds active = simulateCycles (pred rounds) newActive
  where
    activeNeighbors = Set.intersection active . neighbors
    candidates = Set.unions . Set.map neighbors $ active
    inactive = candidates \\ active
    newlyActive = Set.filter ((==) 3 . Set.size . activeNeighbors) inactive
    remainingActive = Set.filter ((\k -> k == 2 || k == 3) . Set.size . activeNeighbors) active
    newActive = Set.union newlyActive remainingActive

activeCubes = Set.size

printResults :: Pocket2Dimension -> PuzzleAnswerPair
printResults initialState = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . activeCubes . simulateCycles 6 . Set.map (from2d :: Coord2D -> Coord3D) $ initialState
    part2 = show . activeCubes . simulateCycles 6 . Set.map (from2d :: Coord2D -> Coord4D) $ initialState

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 17
