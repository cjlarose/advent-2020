module Advent.Parse
  ( Parser
  , parse
  , natural
  , word
  , token
  ) where

import Text.Megaparsec (Parsec, satisfy, some, errorBundlePretty)
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Text.Megaparsec as Megaparsec
import Data.Void (Void)
import Data.Text (Text)
import Data.Char (isSpace)
import Numeric.Natural (Natural)

import Advent.PuzzleAnswerPair (PuzzleAnswerPair)

type Parser = Parsec Void Text

parse :: Parsec Void Text a -> (a -> PuzzleAnswerPair) -> Text -> Either String PuzzleAnswerPair
parse p f x = either (Left . errorBundlePretty) (Right . f) $ Megaparsec.parse p "" x

natural :: Parser Natural
natural = decimal

word :: Parser String
word = some . satisfy $ not . isSpace

token :: Parser a -> Parser a
token p = p <* space
