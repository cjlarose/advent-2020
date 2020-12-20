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
import Control.Applicative.Combinators (between, count, choice, sepBy1)

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse, symbol, word, linesOf)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

data MessageValidity = Valid | Invalid deriving Show

data Replacement = Terminal Char | Alternatives [[Int]] deriving Show
newtype Productions = Productions (IntMap Replacement) deriving Show

token :: Parser a -> Parser a
token p = p <* skipMany (char ' ')

productionsAST :: Parser Productions
productionsAST = Productions . foldr (\(id, r) acc -> IntMap.insert id r acc) IntMap.empty <$> rules
  where
    rules = linesOf rule <* eol
    rule = (,) <$> (decimal <* symbol ":") <*> (literalRule <|> choiceRule)
    literalRule = Terminal <$> token (between (char '"') (char '"') lowerChar)
    choiceRule = Alternatives <$> sepBy1 (some (token decimal)) (symbol "|")

parserForDeterministicCFG :: Productions -> (IntMap (Parser Text), Parser MessageValidity)
parserForDeterministicCFG (Productions rules) = (ruleMap', startRule)
  where
    ruleMap = IntMap.map toParser rules
    toParser :: Replacement -> IntMap (Parser Text) -> Parser Text
    toParser (Terminal c) _ = string . Text.singleton $ c
    toParser (Alternatives options) m = choice . map (try . parserForNonterminalSequence m) $ options
    parserForNonterminalSequence m = foldr (\x acc -> (m ! x) <* acc) (pure "")
    ruleMap' = IntMap.map (\v -> v ruleMap') ruleMap
    startRule = try (Valid <$ ((ruleMap' ! 0) <* eol)) <|> (Invalid <$ (word <* eol))

inputParser :: Parser ([MessageValidity], [MessageValidity])
inputParser = do
  productions <- productionsAST
  let (ruleMap, matchesRule0) = parserForDeterministicCFG productions
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
        count k (ruleMap ! 42)
        rest <- some (ruleMap ! 31)
        if length rest < k
        then eol
        else fail "no parse"
      matchesRule0' = do
        maxK <- length <$> lookAhead word
        let p = choice . map (try . withPrefixSize) $ [maxK,maxK-1..1]
        try (Valid <$ p) <|> (Invalid <$ (word <* eol))
  ((,) <$> lookAhead (some matchesRule0) <*> some matchesRule0') <* eof

printResults :: ([MessageValidity], [MessageValidity]) -> PuzzleAnswerPair
printResults (messagesBeforeChange, messagesAfterChange) = PuzzleAnswerPair (part1, part2)
  where
    isValid Valid = True
    isValid Invalid = False
    part1 = show . length . filter isValid $ messagesBeforeChange
    part2 = show . length . filter isValid $ messagesAfterChange

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 19
