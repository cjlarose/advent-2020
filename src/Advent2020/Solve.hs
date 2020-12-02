module Advent2020.Solve
  ( solve
  ) where

import qualified Data.ByteString.Lazy as LBS
import System.IO (hPutStrLn, stderr)
import Data.Either (Either(..))
import Data.Binary.Put (runPut, putCharUtf8, putStringUtf8)

import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import qualified Advent2020.Day01
import qualified Advent2020.Day02

solver :: Int -> IO (Either String PuzzleAnswerPair)
solver 1 = Advent2020.Day01.solve
solver 2 = Advent2020.Day02.solve
solver n = pure . Left $ "Unknown problem " ++ show n

solve :: Int -> IO LBS.ByteString
solve k = do
  res <- solver k
  let bs = case res of
             Left err -> runPut $ putStringUtf8 err >> putCharUtf8 '\n'
             Right (PuzzleAnswerPair (part1, part2)) -> runPut $ do
                                                          putStringUtf8 part1
                                                          putCharUtf8 '\n'
                                                          putStringUtf8 part2
                                                          putCharUtf8 '\n'
  pure bs
