module Advent.Parse
  ( Parser
  , parse
  ) where

import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as Megaparsec
import Data.Void (Void)
import Data.Text (Text)

import Advent.PuzzleAnswerPair (PuzzleAnswerPair)

type Parser = Parsec Void Text

parse :: Parsec Void Text a -> (a -> PuzzleAnswerPair) -> Text -> Either String PuzzleAnswerPair
parse p f x = either (Left . show) (Right . f) $ Megaparsec.parse p "" x
