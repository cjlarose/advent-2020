module Advent2020.Day06
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, string, alphaNum, endOfLine, digit, hexDigit, space)
import Text.Parsec (many1, sepBy1, eof, (<|>), count, try, choice)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (nonNegativeInteger, word)

inputParser :: Parser [[String]]
inputParser = sepBy1 stringSet endOfLine <* eof
  where
    stringSet = many1 (word <* space)

numQuestions :: [String] -> Int
numQuestions = Set.size . Set.unions . map Set.fromList

numQuestionsAll :: [String] -> Int
numQuestionsAll = Set.size . foldl Set.intersection (Set.fromList ['a'..'z']) . map Set.fromList

printResults :: [[String]] -> PuzzleAnswerPair
printResults groupAnswers = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . sum . (map numQuestions) $ groupAnswers
    part2 = show . sum . (map numQuestionsAll) $ groupAnswers

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 6
