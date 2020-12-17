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

type Coord = (Int, Int, Int)
type PocketDimension = Set Coord

inputParser :: Parser PocketDimension
inputParser = toSet <$> linesOf row
  where
    row = many1 seat
    seat = (Nothing <$ char '.') <|> (recordPos <* char '#')
    recordPos = do
      pos <- getPosition
      let line = sourceLine pos
      let col = sourceColumn pos
      pure . Just $ (line - 1, col - 1)
    toSet xs = Set.fromList . map (\(i, j) -> (j, i, 0)) . catMaybes . concat $ xs

neighbors :: Coord -> Set Coord
neighbors coord@(x, y, z) = Set.fromList $ do
  newX <- [x-1..x+1]
  newY <- [y-1..y+1]
  newZ <- [z-1..z+1]
  let neighbor = (newX, newY, newZ)
  guard $ neighbor /= coord
  pure neighbor

simulateCycles :: Int -> PocketDimension -> PocketDimension
simulateCycles 0 active = active
simulateCycles rounds active = simulateCycles (pred rounds) newActive
  where
    activeNeighbors = Set.intersection active . neighbors
    candidates = Set.unions . Set.map neighbors $ active
    inactive = candidates \\ active
    newlyActive = Set.filter ((==) 3 . Set.size . activeNeighbors) inactive
    remainingActive = Set.filter ((\k -> k == 2 || k == 3) . Set.size . activeNeighbors) active
    newActive = Set.union newlyActive remainingActive

activeCubes :: PocketDimension -> Int
activeCubes = Set.size

printResults :: PocketDimension -> PuzzleAnswerPair
printResults initialState = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . activeCubes . simulateCycles 6 $ initialState
    part2 = "not implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 17
