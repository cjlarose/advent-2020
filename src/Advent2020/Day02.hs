module Advent2020.Day02
  ( solve
  ) where

import Text.Parsec.ByteString (Parser)
import Text.Parsec (many1)
import Text.Parsec.Char (string, lower, char)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf, nonNegativeInteger)

type Password = String
data PasswordPolicy = PasswordPolicy { minOccurences :: Int
                                     , maxOccurences :: Int
                                     , character :: Char } deriving Show
data PasswordLine = PasswordLine PasswordPolicy Password

passwordPolicy :: Parser PasswordPolicy
passwordPolicy = PasswordPolicy <$> nonNegativeInteger <* char '-' <*> nonNegativeInteger <* char ' ' <*> lower

passwordLine :: Parser PasswordLine
passwordLine = PasswordLine <$> passwordPolicy <* string ": " <*> many1 lower

passwordLines :: Parser [PasswordLine]
passwordLines = linesOf passwordLine

checkValid :: PasswordLine -> Bool
checkValid (PasswordLine (PasswordPolicy minOccurences maxOccurences character) password) = occurences <= maxOccurences && occurences >= minOccurences
  where
    occurences = length . filter (character ==) $ password

checkValidPositions :: PasswordLine -> Bool
checkValidPositions (PasswordLine (PasswordPolicy i j character) password) = (1 ==) . length . filter id . zipWith (\index c -> c == character && (index == i || index == j)) [1..] $ password

printResults :: [PasswordLine] -> PuzzleAnswerPair
printResults lines = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . length . filter checkValid $ lines
    part2 = show . length . filter checkValidPositions $ lines

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse passwordLines printResults <$> getProblemInputAsByteString 2
