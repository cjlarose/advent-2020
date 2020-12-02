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
type PasswordLine = (PasswordPolicy, Password)

passwordPolicy :: Parser PasswordPolicy
passwordPolicy = PasswordPolicy <$> nonNegativeInteger <* char '-' <*> nonNegativeInteger <* char ' ' <*> lower

passwordLine :: Parser PasswordLine
passwordLine = (\a b -> (a, b)) <$> passwordPolicy <* string ": " <*> many1 lower

passwordLines :: Parser [PasswordLine]
passwordLines = linesOf passwordLine

checkValid :: PasswordPolicy -> Password -> Bool
checkValid (PasswordPolicy minOccurences maxOccurences character) password = occurences <= maxOccurences && occurences >= minOccurences
  where
    occurences = length . filter (character ==) $ password

checkValidPositions :: PasswordPolicy -> Password -> Bool
checkValidPositions (PasswordPolicy i j character) = (1 ==) . length . filter id . zipWith (\index c -> c == character && (index == i || index == j)) [1..]

printResults :: [PasswordLine] -> PuzzleAnswerPair
printResults lines = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . length . filter (uncurry checkValid) $ lines
    part2 = show . length . filter (uncurry checkValidPositions) $ lines

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse passwordLines printResults <$> getProblemInputAsByteString 2
