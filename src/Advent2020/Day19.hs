{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day19
  ( solve
  ) where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, (!))
import qualified Data.Text as Text
import Data.Text (Text)
import Text.Megaparsec (eof, (<|>), try, some, skipMany, lookAhead)
import Text.Megaparsec.Char (eol, char, string, lowerChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Applicative.Combinators (between, count, choice)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(InfixL))

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse, symbol, word, linesOf)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

data MessageValidity = Valid | Invalid deriving Show

inputParser :: Parser ([MessageValidity], [MessageValidity])
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
      -- 0 is the only rule that uses 8 and 11
      -- let's just replace 0
      -- 0: 8 11
      -- 8: 42 | 42 8 -- any number of 42s repeated, at least one
      -- 11: 42 31 | 42 11 31 -- balanced 42 and 31, at least one pair
      --
      -- 0: some number of 42s <* (equal number 42s and 31s, at least one pair)
      -- 0: (k, k >= 1) number of 42s <* (z, 1 <= z < k) number of 31s
      -- try k = n first, then go down from there, where n is the length of the message
      withPrefixSize k = do
        count k (ruleMap' ! 42)
        rest <- some (ruleMap' ! 31)
        if length rest < k
        then pure . head $ rest
        else fail "no parse"
      rule0' = do
        maxK <- length <$> lookAhead word
        choice . map (try . withPrefixSize) $ [maxK,maxK-1..1]
      message :: Parser MessageValidity
      message = try (Valid <$ (rule0 <* eol)) <|> (Invalid <$ (word <* eol))
      message' :: Parser MessageValidity
      message' = try (Valid <$ (rule0' <* eol)) <|> (Invalid <$ (word <* eol))
  ((,) <$> lookAhead (some message) <*> some message') <* eof

printResults :: ([MessageValidity], [MessageValidity]) -> PuzzleAnswerPair
printResults (messagesBeforeChange, messagesAfterChange) = PuzzleAnswerPair (part1, part2)
  where
    isValid Valid = True
    isValid Invalid = False
    part1 = show . length . filter isValid $ messagesBeforeChange
    part2 = show . length . filter isValid $ messagesAfterChange

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 19
