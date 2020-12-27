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
import qualified Advent2020.Day12
import qualified Advent2020.Day13
import qualified Advent2020.Day14
import qualified Advent2020.Day15
import qualified Advent2020.Day16
import qualified Advent2020.Day17
import qualified Advent2020.Day18
import qualified Advent2020.Day19
import qualified Advent2020.Day20
import qualified Advent2020.Day21
import qualified Advent2020.Day22
import qualified Advent2020.Day23
import qualified Advent2020.Day24
import qualified Advent2020.Day25

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
solverForProblem 12 = Advent2020.Day12.solve
solverForProblem 13 = Advent2020.Day13.solve
solverForProblem 14 = Advent2020.Day14.solve
solverForProblem 15 = Advent2020.Day15.solve
solverForProblem 16 = Advent2020.Day16.solve
solverForProblem 17 = Advent2020.Day17.solve
solverForProblem 18 = Advent2020.Day18.solve
solverForProblem 19 = Advent2020.Day19.solve
solverForProblem 20 = Advent2020.Day20.solve
solverForProblem 21 = Advent2020.Day21.solve
solverForProblem 22 = Advent2020.Day22.solve
solverForProblem 23 = Advent2020.Day23.solve
solverForProblem 24 = Advent2020.Day24.solve
solverForProblem 25 = Advent2020.Day25.solve
solverForProblem n = pure . Left $ "Unknown problem " ++ show n
