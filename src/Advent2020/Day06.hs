module Advent2020.Day06
  ( solve
  ) where

import qualified Data.Set as Set
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (endOfLine, space)
import Text.Parsec (many1, sepBy1, eof)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (word)

type GroupAnswers = [String]

inputParser :: Parser [GroupAnswers]
inputParser = sepBy1 (many1 (word <* space)) endOfLine <* eof

numQuestionsAny :: GroupAnswers -> Int
numQuestionsAny = Set.size . Set.unions . map Set.fromList

numQuestionsAll :: GroupAnswers -> Int
numQuestionsAll = Set.size . foldl Set.intersection (Set.fromList ['a'..'z']) . map Set.fromList

printResults :: [GroupAnswers] -> PuzzleAnswerPair
printResults groups = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . sum . map numQuestionsAny $ groups
    part2 = show . sum . map numQuestionsAll $ groups

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 6
