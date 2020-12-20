{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day19
  ( solve
  ) where

import Debug.Trace (traceShow)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, (!))
import Text.Megaparsec (eof, (<|>), try, some, skipMany, lookAhead)
import Text.Megaparsec.Char (eol, char, lowerChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Applicative.Combinators (between, count, choice, sepBy1)

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse, symbol, word, linesOf)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

data MessageValidity = Valid [String] | Invalid String deriving Show

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

parserForNondeterministicCFG :: Productions -> (IntMap (Parser [String]), Parser [String])
parserForNondeterministicCFG (Productions rules) = (ruleMap', startRule)
  where
    ruleMap :: IntMap (IntMap (Parser [String]) -> Parser [String])
    ruleMap = IntMap.map toParser rules
    toParser :: Replacement -> IntMap (Parser [String]) -> Parser [String]
    toParser (Terminal c) _ = try (pure . pure <$> char c) <|> pure []
    toParser (Alternatives options) m = choice . map (try . parserForNonterminalSequence m) $ options
    parserForNonterminalSequence :: IntMap (Parser [String]) -> [Int] -> Parser [String]
    parserForNonterminalSequence m = foldr ((\p acc -> (\xs ys -> [x ++ y | x <- (traceShow ("lhs", xs, "rhs", ys) xs), y <- ys]) <$> p <*> acc) . (m !)) (pure [""])
    ruleMap' :: IntMap (Parser [String])
    ruleMap' = IntMap.map (\v -> v ruleMap') ruleMap
    --startRule = try (Valid <$> ((ruleMap' ! 0) <* eol)) <|> (Invalid <$> (word <* eol))
    startRule :: Parser [String]
    startRule = ruleMap' ! 0

inputParser :: Parser ([MessageValidity], [MessageValidity])
inputParser = do
  productions <- productionsAST
  let (_, matchesRule0) = parserForNondeterministicCFG productions
      -- 0 is the only rule that uses 8 and 11
      -- let's just replace 0
      -- 0: 8 11
      -- 8: 42 | 42 8 -- any number of 42s repeated, at least one
      -- 11: 42 31 | 42 11 31 -- balanced 42 and 31, at least one pair
      --
      -- 0: some number of 42s <* (equal number 42s and 31s, at least one pair)
      -- 0: (k, k >= 1) number of 42s <* (z, 1 <= z < k) number of 31s
      -- try k = n first, then go down from there, where n is the length of the message
      -- withPrefixSize :: Int -> Parser [String]
      -- withPrefixSize k = do
      --   prefix <- count k (ruleMap ! 42)
      --   rest <- some (ruleMap ! 31)
      --   if length rest < k
      --   then concat (prefix ++ rest) <$ eol
      --   else fail "no parse"
      -- matchesRule0' :: Parser MessageValidity
      -- matchesRule0' = do
      --   maxK <- length <$> lookAhead word
      --   let p = choice . map (try . withPrefixSize) $ [maxK,maxK-1..1]
      --   try (Valid <$> p) <|> (Invalid <$> (word <* eol))
      validate :: Parser [String] -> Parser MessageValidity
      validate p = do
        let f xs = if null (traceShow ("validate", xs) xs) then Invalid <$> (word <* eol) else pure . Valid $ xs
        try (p <* eol >>= f) <|> (Invalid <$> (word <* eol))
  ((,) <$> some (validate matchesRule0) <*> pure []) <* eof

printResults :: ([MessageValidity], [MessageValidity]) -> PuzzleAnswerPair
printResults (messagesBeforeChange, messagesAfterChange) = PuzzleAnswerPair (part1, part2)
  where
    isValid (Valid _) = True
    isValid (Invalid _) = False
    part1 = show . length . filter isValid $ map (\x -> traceShow x x) messagesBeforeChange
    part2 = show . length . filter isValid $ map (\x -> traceShow x x) messagesAfterChange

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 19
