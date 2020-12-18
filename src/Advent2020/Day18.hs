module Advent2020.Day18
  ( solve
  ) where

import Text.Parsec.ByteString (Parser)
import Text.Parsec (try, char, (<|>), between, chainl1, lookAhead)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf, integerWithOptionalLeadingSign)

data Operator = Plus | Times deriving Show
data Expression = BinaryExpression Expression Operator Expression
                | Literal Integer
                deriving Show

inputParser :: Parser ([Expression], [Expression])
inputParser = (,) <$> lookAhead (linesOf binaryExpression) <*> linesOf binaryExpression'
  where
    binaryExpression = chainl1 term (char ' ' *> operator <* char ' ')
    term = literalExpression <|> parenthesizedExpression
    parenthesizedExpression = between (char '(') (char ')') binaryExpression
    operator = mulOp <|> addOp
    literalExpression = Literal <$> integerWithOptionalLeadingSign

    binaryExpression' = chainl1 factor (try (char ' ' *> mulOp <* char ' '))
    factor = chainl1 term' (try (char ' ' *> addOp <* char ' '))
    term' = literalExpression <|> parenthesizedExpression'
    parenthesizedExpression' = between (char '(') (char ')') binaryExpression'
    mulOp = (`BinaryExpression` Times) <$ char '*'
    addOp = (`BinaryExpression` Plus) <$ char '+'

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
