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
data ShipState = ShipState { getDirection :: Direction
                           , getPosition :: (Int, Int)
                           }

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

translate :: (Int, Int) -> Direction -> Natural -> (Int, Int)
translate (i, j) N x = (i - fromIntegral x, j)
translate (i, j) E x = (i, j + fromIntegral x)
translate (i, j) S x = (i + fromIntegral x, j)
translate (i, j) W x = (i, j - fromIntegral x)

moveShipByInstructions :: [Instruction] -> (Int, Int)
moveShipByInstructions = getPosition . foldl' moveShip ShipState{ getDirection=E, getPosition=(0,0) }
  where
    moveShip :: ShipState -> Instruction -> ShipState
    moveShip s@ShipState{getDirection=dir} (Move F x) = s { getPosition = getPosition $ moveShip s (Move dir x) }
    moveShip s@ShipState{getPosition=pos} (Move dir x) = s { getPosition=translate pos dir x }

    moveShip s@ShipState{getDirection=N} (RotateLeft 90) = s { getDirection=W }
    moveShip s@ShipState{getDirection=E} (RotateLeft 90) = s { getDirection=N }
    moveShip s@ShipState{getDirection=S} (RotateLeft 90) = s { getDirection=E }
    moveShip s@ShipState{getDirection=W} (RotateLeft 90) = s { getDirection=S }

    moveShip s@ShipState{getDirection=N} (RotateLeft 180) = s { getDirection=S }
    moveShip s@ShipState{getDirection=E} (RotateLeft 180) = s { getDirection=W }
    moveShip s@ShipState{getDirection=S} (RotateLeft 180) = s { getDirection=N }
    moveShip s@ShipState{getDirection=W} (RotateLeft 180) = s { getDirection=E }

    moveShip s (RotateLeft 270) = moveShip s (RotateRight 90)

    moveShip s@ShipState{getDirection=N} (RotateRight 90) = s { getDirection=E }
    moveShip s@ShipState{getDirection=E} (RotateRight 90) = s { getDirection=S }
    moveShip s@ShipState{getDirection=S} (RotateRight 90) = s { getDirection=W }
    moveShip s@ShipState{getDirection=W} (RotateRight 90) = s { getDirection=N }

    moveShip s@ShipState{getDirection=N} (RotateRight 180) = s { getDirection=S }
    moveShip s@ShipState{getDirection=E} (RotateRight 180) = s { getDirection=W }
    moveShip s@ShipState{getDirection=S} (RotateRight 180) = s { getDirection=N }
    moveShip s@ShipState{getDirection=W} (RotateRight 180) = s { getDirection=E }

    moveShip s (RotateRight 270) = moveShip s (RotateLeft 90)

    moveShip _ instruction = error $ "cannot handle instruction " ++ show instruction

moveShipByWaypointInstructions :: [Instruction] -> (Int, Int)
moveShipByWaypointInstructions = getPosition . fst . foldl' moveShip' (ShipState{ getDirection=E, getPosition=(0,0) }, (-1, 10))
  where
    moveShip' :: (ShipState, (Int, Int)) -> Instruction -> (ShipState, (Int, Int))
    moveShip' (s@ShipState{getPosition=(i,j)}, (di, dj)) (Move F x) = (s { getPosition=(i + di * fromIntegral x, j + dj * fromIntegral x) }, (di, dj))
    moveShip' (s, waypoint) (Move dir x) = (s, translate waypoint dir x)

    moveShip' (s, (i, j)) (RotateLeft 90) = (s, (- j, i))
    moveShip' (s, (i, j)) (RotateLeft 180) = (s, (- i, - j))
    moveShip' (s, (i, j)) (RotateLeft 270) = (s, (j, - i))

    moveShip' (s, (i, j)) (RotateRight 90) = (s, (j, - i))
    moveShip' (s, (i, j)) (RotateRight 180) = (s, (- i, - j))
    moveShip' (s, (i, j)) (RotateRight 270) = (s, (- j, i))

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (i, j) = abs i + abs j

printResults :: [Instruction] -> PuzzleAnswerPair
printResults instructions = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . manhattanDistance . moveShipByInstructions $ instructions
    part2 = show . manhattanDistance . moveShipByWaypointInstructions $ instructions

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 12
