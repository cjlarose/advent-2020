module Advent.Input
  ( getProblemInputAsString
  , getProblemInputAsByteString
  , getProblemInputAsText
  , withSuccessfulParse
  ) where

import qualified Data.ByteString as B
import qualified Text.Parsec (parse)
import Text.Parsec.ByteString (Parser)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Advent.PuzzleAnswerPair (PuzzleAnswerPair)

path :: Int -> String
path problemNumber = "inputs/" ++ show problemNumber ++ ".txt"

getProblemInputAsString :: Int -> IO String
getProblemInputAsString = readFile . path

getProblemInputAsByteString :: Int -> IO B.ByteString
getProblemInputAsByteString = B.readFile . path

getProblemInputAsText :: Int -> IO Text
getProblemInputAsText problemNumber = decodeUtf8 <$> getProblemInputAsByteString problemNumber

withSuccessfulParse :: Parser a -> (a -> PuzzleAnswerPair) -> B.ByteString -> Either String PuzzleAnswerPair
withSuccessfulParse p f x = either (Left . show) (Right . f) $ Text.Parsec.parse p "" x
