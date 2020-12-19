{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day19
  ( solve
  ) where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, (!))
import qualified Data.Text as Text
import Data.Text (Text)
import Text.Megaparsec (eof, (<|>), try, some, skipMany)
import Text.Megaparsec.Char (eol, char, string, lowerChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Applicative.Combinators (between)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(InfixL))

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse, symbol, word, linesOf)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

data MessageValidity = Valid | Invalid deriving Show

inputParser :: Parser [MessageValidity]
inputParser = do
  let token :: Parser a -> Parser a
      token p = p <* skipMany (char ' ')
      rule :: Parser (Int, IntMap (Parser Text) -> Parser Text)
      rule = (,) <$> (decimal <* symbol ":") <*> (literalRule <|> choiceRule)
      literalRule :: Parser (IntMap (Parser Text) -> Parser Text)
      literalRule = const . string . Text.singleton <$> token (between (char '"') (char '"') lowerChar)
      choiceRule :: Parser (IntMap (Parser Text) -> Parser Text)
      choiceRule = makeExprParser term [ [ InfixL alternationOp ] ]
      ruleFor :: Int -> IntMap (Parser Text) -> Parser Text
      ruleFor i m = m ! i
      term :: Parser (IntMap (Parser Text) -> Parser Text)
      term = (\xs m -> foldr (\i acc -> ruleFor i m <* acc) (pure "") xs) <$> some (token decimal)
      alternationOp = (\f g m -> try (f m) <|> g m) <$ symbol "|"
  rules <- linesOf rule <* eol
  let ruleMap :: IntMap (IntMap (Parser Text) -> Parser Text)
      ruleMap = foldr (\(i, p) acc -> IntMap.insert i p acc) IntMap.empty rules
      ruleMap' = IntMap.map (\v -> v ruleMap') ruleMap
      rule0 = ruleMap' ! 0
      message :: Parser MessageValidity
      message = try (Valid <$ (rule0 <* eol)) <|> (Invalid <$ (word <* eol))
  some message <* eof

printResults :: [MessageValidity] -> PuzzleAnswerPair
printResults messages = PuzzleAnswerPair (part1, part2)
  where
    isValid Valid = True
    isValid Invalid = False
    part1 = show . length . filter isValid $ messages
    part2 = "not implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 19
