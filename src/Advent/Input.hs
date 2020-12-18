module Advent.Input
  ( getProblemInputAsString
  , getProblemInputAsByteString
  , withSuccessfulParse
  , withTwoSuccessfulParses
  ) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Data.Either (either)

import qualified Data.ByteString as B
import qualified Text.Parsec (parse)
import Text.Parsec (lookAhead)
import Text.Parsec.ByteString (Parser)

import Advent.PuzzleAnswerPair (PuzzleAnswerPair)

path :: Int -> String
path problemNumber = "inputs/" ++ show problemNumber ++ ".txt"

getProblemInputAsString :: Int -> IO String
getProblemInputAsString = readFile . path

getProblemInputAsByteString :: Int -> IO B.ByteString
getProblemInputAsByteString = B.readFile . path

withSuccessfulParse :: Parser a -> (a -> PuzzleAnswerPair) -> B.ByteString -> Either String PuzzleAnswerPair
withSuccessfulParse p f x = either (Left . show) (Right . f) $ Text.Parsec.parse p "" x

withTwoSuccessfulParses :: Parser a -> Parser b -> (a -> b -> PuzzleAnswerPair) -> B.ByteString -> Either String PuzzleAnswerPair
withTwoSuccessfulParses pa pb = withSuccessfulParse ((\a b -> (a, b)) <$> lookAhead pa <*> pb) . uncurry
