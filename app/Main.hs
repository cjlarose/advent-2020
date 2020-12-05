module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, putStrLn)
import System.Exit (exitFailure)

import Advent2020.Solve (solver)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> do
      let n = read arg
      result <- solver n
      case result of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right (PuzzleAnswerPair (part1, part2)) -> do
          let output = part1 ++ "\n" ++ part2
          putStrLn output
    _ -> hPutStrLn stderr "Usage: advent2020-exe problem-number"
