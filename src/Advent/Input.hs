module Advent.Input
  ( getProblemInputAsString
  , getProblemInputAsByteString
  , withSuccessfulParse
  ) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import qualified Data.ByteString as B
import Text.Parsec (ParseError)
import qualified Text.Parsec (parse)
import Text.Parsec.ByteString (Parser)

path :: Int -> String
path problemNumber = "inputs/" ++ (show problemNumber) ++ ".txt"

getProblemInputAsString :: Int -> IO String
getProblemInputAsString = readFile . path

getProblemInputAsByteString :: Int -> IO B.ByteString
getProblemInputAsByteString = B.readFile . path

withSuccessfulParse :: Parser a -> (a -> (String, String)) -> B.ByteString -> Either String (String, String)
withSuccessfulParse p f x = let res = Text.Parsec.parse p "" x
  in case res of
       Left err -> Left $ show err
       Right parsed -> Right . f $ parsed
