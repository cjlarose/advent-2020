module Advent2020.Day18
  ( solve
  ) where

import Text.Parsec.ByteString (Parser)
import Text.Parsec (try, char, (<|>), between, chainl1, parse)

import Advent.Input (getProblemInputAsByteString)
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

inputParser' :: Parser [Expression]
inputParser' = linesOf binaryExpression
  where
    binaryExpression = chainl1 factor (try (char ' ' *> mulOp <* char ' '))
    factor = chainl1 term (try (char ' ' *> addOp <* char ' '))
    term = literalExpression <|> parenthesizedExpression
    parenthesizedExpression = between (char '(') (char ')') binaryExpression
    mulOp = (`BinaryExpression` Times) <$ char '*'
    addOp = (`BinaryExpression` Plus) <$ char '+'
    literalExpression = Literal <$> integerWithOptionalLeadingSign

evaluate :: Expression -> Integer
evaluate (Literal x) = x
evaluate (BinaryExpression lhs Plus rhs) = evaluate lhs + evaluate rhs
evaluate (BinaryExpression lhs Times rhs) = evaluate lhs * evaluate rhs

solve :: IO (Either String PuzzleAnswerPair)
solve = do
  input <- getProblemInputAsByteString 18
  case parse inputParser "" input of
    Left err -> pure . Left . show $ err
    Right expressions -> do
      let part1 = show . sum . map evaluate $ expressions
      case parse inputParser' "" input of
        Left err -> pure . Left . show $ err
        Right expressions' -> do
          let part2 = show . sum . map evaluate $ expressions'
          pure . Right . PuzzleAnswerPair $ (part1, part2)
