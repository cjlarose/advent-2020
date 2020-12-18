module Advent2020.Day18
  ( solve
  ) where

import Text.Parsec.ByteString (Parser)
import Text.Parsec (char, (<|>), between, chainl1)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf, integerWithOptionalLeadingSign)

data Operator = Plus | Times deriving Show
data Expression = BinaryExpression Expression Operator Expression
                | Literal Integer
                deriving Show

inputParser :: Parser [Expression]
inputParser = linesOf binaryExpression
  where
    binaryExpression = chainl1 term (char ' ' *> operator <* char ' ')
    term = literalExpression <|> parenthesizedExpression
    parenthesizedExpression = between (char '(') (char ')') binaryExpression
    operator :: Parser (Expression -> Expression -> Expression)
    operator = ((`BinaryExpression` Plus) <$ char '+') <|> ((`BinaryExpression` Times) <$ char '*')
    literalExpression = Literal <$> integerWithOptionalLeadingSign

evaluate :: Expression -> Integer
evaluate (Literal x) = x
evaluate (BinaryExpression lhs Plus rhs) = evaluate lhs + evaluate rhs
evaluate (BinaryExpression lhs Times rhs) = evaluate lhs * evaluate rhs

printResults :: [Expression] -> PuzzleAnswerPair
printResults expressions = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . sum . map evaluate $ expressions
    part2 = "not implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 18
