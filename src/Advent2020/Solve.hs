module Advent2020.Solve
  ( solverForProblem
  ) where

import Data.Either (Either(..))

import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import qualified Advent2020.Day01
import qualified Advent2020.Day02
import qualified Advent2020.Day03
import qualified Advent2020.Day04

solverForProblem :: Int -> IO (Either String PuzzleAnswerPair)
solverForProblem 1 = Advent2020.Day01.solve
solverForProblem 2 = Advent2020.Day02.solve
solverForProblem 3 = Advent2020.Day03.solve
solverForProblem 4 = Advent2020.Day04.solve
solverForProblem n = pure . Left $ "Unknown problem " ++ show n
