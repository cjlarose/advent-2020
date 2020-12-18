module Advent2020.Day18
  ( solve
  ) where

import Text.Parsec.ByteString (Parser)
import Text.Parsec (char, (<|>), between, chainl1, lookAhead, spaces, many1)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integerWithOptionalLeadingSign)

data Operator = Plus | Times deriving Show
data Expression = BinaryExpression Expression Operator Expression
                | Literal Integer
                deriving Show

inputParser :: Parser ([Expression], [Expression])
inputParser = (,) <$> lookAhead (many1 binaryExpression) <*> many1 mulExpression
  where
    token :: Parser a -> Parser a
    token p = p <* spaces
    symbol = token . char

    binaryExpression = chainl1 term operator
    term = literalExpression <|> parenthesizedExpression
    parenthesizedExpression = between (symbol '(') (symbol ')') binaryExpression
    operator = mulOp <|> addOp
    literalExpression = Literal <$> token integerWithOptionalLeadingSign

    mulExpression = chainl1 factor mulOp
    factor = chainl1 addend addOp
    addend = literalExpression <|> parenthesizedMulExpression
    parenthesizedMulExpression = between (symbol '(') (symbol ')') mulExpression
    mulOp = (`BinaryExpression` Times) <$ symbol '*'
    addOp = (`BinaryExpression` Plus) <$ symbol '+'

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
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 18
