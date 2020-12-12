module Advent2020.Day12
  ( solve
  ) where

import Numeric.Natural (Natural)
import Data.List (foldl')
import Text.Parsec.ByteString (Parser)
import Text.Parsec (choice, char)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf, natural)

data Direction = N | E | S | W | F deriving Show
data Instruction = Move Direction Natural | RotateLeft Natural | RotateRight Natural deriving Show

inputParser :: Parser [Instruction]
inputParser = linesOf instruction
  where
    instruction = choice [ action 'N' $ Move N
                         , action 'E' $ Move E
                         , action 'S' $ Move S
                         , action 'W' $ Move W
                         , action 'L' RotateLeft
                         , action 'R' RotateRight
                         , action 'F' $ Move F
                         ]
    action c f = f <$> (char c *> natural)

data ShipState = ShipState { getDirection :: Direction
                           , getPosition :: (Int, Int)
                           }

moveShip :: [Instruction] -> (Int, Int)
moveShip = getPosition . foldl' f ShipState{ getDirection=E, getPosition=(0,0) }
  where
    f :: ShipState -> Instruction -> ShipState
    f s@ShipState{getPosition=(i, j)} (Move N x) = s { getPosition=(i - fromIntegral x, j) }
    f s@ShipState{getPosition=(i, j)} (Move E x) = s { getPosition=(i, j + fromIntegral x) }
    f s@ShipState{getPosition=(i, j)} (Move S x) = s { getPosition=(i + fromIntegral x, j) }
    f s@ShipState{getPosition=(i, j)} (Move W x) = s { getPosition=(i, j - fromIntegral x) }

    f s@ShipState{getDirection=N} (RotateLeft 90) = s { getDirection=W }
    f s@ShipState{getDirection=E} (RotateLeft 90) = s { getDirection=N }
    f s@ShipState{getDirection=S} (RotateLeft 90) = s { getDirection=E }
    f s@ShipState{getDirection=W} (RotateLeft 90) = s { getDirection=S }

    f s@ShipState{getDirection=N} (RotateLeft 180) = s { getDirection=S }
    f s@ShipState{getDirection=E} (RotateLeft 180) = s { getDirection=W }
    f s@ShipState{getDirection=S} (RotateLeft 180) = s { getDirection=N }
    f s@ShipState{getDirection=W} (RotateLeft 180) = s { getDirection=E }

    f s (RotateLeft 270) = f s (RotateRight 90)

    f s@ShipState{getDirection=N} (RotateRight 90) = s { getDirection=E }
    f s@ShipState{getDirection=E} (RotateRight 90) = s { getDirection=S }
    f s@ShipState{getDirection=S} (RotateRight 90) = s { getDirection=W }
    f s@ShipState{getDirection=W} (RotateRight 90) = s { getDirection=N }

    f s@ShipState{getDirection=N} (RotateRight 180) = s { getDirection=S }
    f s@ShipState{getDirection=E} (RotateRight 180) = s { getDirection=W }
    f s@ShipState{getDirection=S} (RotateRight 180) = s { getDirection=N }
    f s@ShipState{getDirection=W} (RotateRight 180) = s { getDirection=E }

    f s (RotateRight 270) = f s (RotateLeft 90)

    f s@ShipState{getDirection=dir} (Move F x) = s { getPosition = getPosition $ f s (Move dir x) }

    f _ instruction = error $ "cannot handle instruction " ++ show instruction

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (i, j) = abs i + abs j

printResults :: [Instruction] -> PuzzleAnswerPair
printResults instructions = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . manhattanDistance . moveShip $ instructions
    part2 = "not yet implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 12
