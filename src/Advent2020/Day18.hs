{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day18
  ( solve
  ) where

import Text.Megaparsec ((<|>), lookAhead, some, eof)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(InfixL))

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse, token, symbol, parens)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

data BinaryOperator = Plus | Times deriving Show
data Expression = BinaryExpression Expression BinaryOperator Expression
                | Literal Integer
                deriving Show

inputParser :: Parser ([Expression], [Expression])
inputParser = (,) <$> lookAhead (some equalPredExpression <* eof) <*> (some additionFirstExpression <* eof)
  where
    literalExpression = Literal <$> token decimal
    mulOp = (`BinaryExpression` Times) <$ symbol "*"
    addOp = (`BinaryExpression` Plus) <$ symbol "+"

    equalPredExpression = makeExprParser term equalPredOpTable
    equalPredOpTable = [ [ InfixL addOp , InfixL mulOp ] ]
    term = literalExpression <|> parenthesizedExpression
    parenthesizedExpression = parens equalPredExpression

    additionFirstExpression = makeExprParser term' additionFirstOpTable
    additionFirstOpTable = [ [ InfixL addOp ]
                           , [ InfixL mulOp ] ]
    term' = literalExpression <|> parenthesizedMulExpression
    parenthesizedMulExpression = parens additionFirstExpression

evaluate :: Expression -> Integer
evaluate (Literal x) = x
evaluate (BinaryExpression lhs Plus rhs) = evaluate lhs + evaluate rhs
evaluate (BinaryExpression lhs Times rhs) = evaluate lhs * evaluate rhs

printResults :: ([Expression], [Expression]) -> PuzzleAnswerPair
printResults (equalPredTree, addFirstTree) = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . sum . map evaluate $ equalPredTree
    part2 = show . sum . map evaluate $ addFirstTree

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 18
