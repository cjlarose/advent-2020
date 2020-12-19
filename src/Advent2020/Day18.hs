module Advent2020.Day18
  ( solve
  ) where

import Text.Megaparsec (Parsec, (<|>), between, lookAhead, some)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(InfixL))

import Advent.Input (getProblemInputAsText, withSuccessfulParse')
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Data.Text (Text)
import Data.Void (Void)

type Parser = Parsec Void Text

data BinaryOperator = Plus | Times deriving Show
data Expression = BinaryExpression Expression BinaryOperator Expression
                | Literal Integer
                deriving Show

inputParser :: Parser ([Expression], [Expression])
inputParser = (,) <$> lookAhead (some equalPredExpression) <*> some additionFirstExpression
  where
    token :: Parser a -> Parser a
    token p = p <* space
    symbol = token . char

    literalExpression = Literal <$> token decimal
    mulOp = (`BinaryExpression` Times) <$ symbol '*'
    addOp = (`BinaryExpression` Plus) <$ symbol '+'

    equalPredExpression = makeExprParser term equalPredOpTable
    equalPredOpTable = [ [ InfixL addOp , InfixL mulOp ] ]
    term = literalExpression <|> parenthesizedExpression
    parenthesizedExpression = between (symbol '(') (symbol ')') equalPredExpression

    additionFirstExpression = makeExprParser term' additionFirstOpTable
    additionFirstOpTable = [ [ InfixL addOp ]
                           , [ InfixL mulOp ] ]
    term' = literalExpression <|> parenthesizedMulExpression
    parenthesizedMulExpression = between (symbol '(') (symbol ')') additionFirstExpression

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
solve = withSuccessfulParse' inputParser printResults <$> getProblemInputAsText 18
