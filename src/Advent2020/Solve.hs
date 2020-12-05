module Advent2020.Solve
  ( solver
  ) where

import Data.Either (Either(..))

import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import qualified Advent2020.Day01
import qualified Advent2020.Day02
import qualified Advent2020.Day03
import qualified Advent2020.Day04

solver :: Int -> IO (Either String PuzzleAnswerPair)
solver 1 = Advent2020.Day01.solve
solver 2 = Advent2020.Day02.solve
solver 3 = Advent2020.Day03.solve
solver 4 = Advent2020.Day04.solve
solver n = pure . Left $ "Unknown problem " ++ show n
