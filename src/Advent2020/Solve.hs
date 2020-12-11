module Advent2020.Solve
  ( solverForProblem
  ) where

import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import qualified Advent2020.Day01
import qualified Advent2020.Day02
import qualified Advent2020.Day03
import qualified Advent2020.Day04
import qualified Advent2020.Day05
import qualified Advent2020.Day06
import qualified Advent2020.Day07
import qualified Advent2020.Day08
import qualified Advent2020.Day09
import qualified Advent2020.Day10
import qualified Advent2020.Day11

solverForProblem :: Int -> IO (Either String PuzzleAnswerPair)
solverForProblem 1 = Advent2020.Day01.solve
solverForProblem 2 = Advent2020.Day02.solve
solverForProblem 3 = Advent2020.Day03.solve
solverForProblem 4 = Advent2020.Day04.solve
solverForProblem 5 = Advent2020.Day05.solve
solverForProblem 6 = Advent2020.Day06.solve
solverForProblem 7 = Advent2020.Day07.solve
solverForProblem 8 = Advent2020.Day08.solve
solverForProblem 9 = Advent2020.Day09.solve
solverForProblem 10 = Advent2020.Day10.solve
solverForProblem 11 = Advent2020.Day11.solve
solverForProblem n = pure . Left $ "Unknown problem " ++ show n
