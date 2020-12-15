module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import Advent2020.Solve (solverForProblem)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import qualified Advent2020.Day15Drew as Day15Drew

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> do
      let n = read arg
      result <- solverForProblem n
      case result of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right (PuzzleAnswerPair (part1, part2)) -> do
          let output = part1 ++ "\n" ++ part2
          putStrLn output
    _ -> Day15Drew.main
