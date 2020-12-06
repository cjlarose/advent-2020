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

inputParser :: Parser [[String]]
inputParser = sepBy1 stringSet endOfLine <* eof
  where
    stringSet = many1 (word <* space)

numQuestionsAny :: [String] -> Int
numQuestionsAny = Set.size . Set.unions . map Set.fromList

numQuestionsAll :: [String] -> Int
numQuestionsAll = Set.size . foldl Set.intersection (Set.fromList ['a'..'z']) . map Set.fromList

printResults :: [[String]] -> PuzzleAnswerPair
printResults groupAnswers = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . sum . map numQuestionsAny $ groupAnswers
    part2 = show . sum . map numQuestionsAll $ groupAnswers

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 6
