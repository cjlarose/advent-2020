{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day02
  ( solve
  ) where

import Numeric.Natural (Natural)
import Text.Megaparsec (some, eof)
import Text.Megaparsec.Char (char, string, lowerChar)

import Advent.Input (getProblemInputAsText)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.Parse (Parser, parse, natural, token, word)

type Password = String
data PasswordPolicy = PasswordPolicy { minOccurences :: Natural
                                     , maxOccurences :: Natural
                                     , character :: Char } deriving Show
data PasswordLine = PasswordLine PasswordPolicy Password

passwordLines :: Parser [PasswordLine]
passwordLines = some passwordLine <* eof
  where
    passwordLine = PasswordLine <$> passwordPolicy <* string ": " <*> token word
    passwordPolicy = PasswordPolicy <$> natural <* char '-' <*> natural <* char ' ' <*> lowerChar

checkValid :: PasswordLine -> Bool
checkValid (PasswordLine (PasswordPolicy minOccurences maxOccurences character) password) = occurences <= maxOccurences && occurences >= minOccurences
  where
    occurences = fromIntegral . length . filter (character ==) $ password

checkValidPositions :: PasswordLine -> Bool
checkValidPositions (PasswordLine (PasswordPolicy i j character) password) = (1 ==) . length . filter id . zipWith (\index c -> c == character && (index == i || index == j)) [1..] $ password

printResults :: [PasswordLine] -> PuzzleAnswerPair
printResults lines = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . length . filter checkValid $ lines
    part2 = show . length . filter checkValidPositions $ lines

solve :: IO (Either String PuzzleAnswerPair)
solve = parse passwordLines printResults <$> getProblemInputAsText 2
