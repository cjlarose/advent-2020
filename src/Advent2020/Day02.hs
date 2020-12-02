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
data PasswordLine = ValidPassword PasswordPolicy Password
                  | InvalidPassword PasswordPolicy Password deriving Show

passwordPolicy :: Parser PasswordPolicy
passwordPolicy = PasswordPolicy <$> (nonNegativeInteger <* char '-') <*> (nonNegativeInteger <* char ' ') <*> lower

checkValid :: PasswordPolicy -> Password -> Bool
checkValid (PasswordPolicy minOccurences maxOccurences character) password = occurences <= maxOccurences && occurences >= minOccurences
  where
    occurences = length . filter (character ==) $ password

passwordLine :: Parser PasswordLine
passwordLine = markValid <$> (passwordPolicy <* string ": ") <*> many1 lower
  where
    markValid policy password | checkValid policy password = ValidPassword policy password
                              | otherwise = InvalidPassword policy password

passwordLines :: Parser [PasswordLine]
passwordLines = linesOf passwordLine

isValid :: PasswordLine -> Bool
isValid (ValidPassword _ _ ) = True
isValid (InvalidPassword _ _ ) = False

printResults :: [PasswordLine] -> PuzzleAnswerPair
printResults lines = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . length . filter isValid $ lines
    part2 = "not yet implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse passwordLines printResults <$> getProblemInputAsByteString 2
