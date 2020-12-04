module Advent2020.Day04
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (string, lower, char, alphaNum, endOfLine)
import Text.Parsec (many1, sepBy1, sepEndBy1, eof, (<|>))

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf, nonNegativeInteger)

type Passport = Map String String

requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validPassport :: Passport -> Bool
validPassport = Set.isSubsetOf (Set.fromList requiredKeys) . Map.keysSet

passportsP :: Parser [Passport]
passportsP = sepBy1 passport endOfLine <* eof
  where
    passport :: Parser Passport
    passport = Map.fromList <$> sepEndBy1 field (endOfLine <|> char ' ' )
    field :: Parser (String, String)
    field = (\a b -> (a, b)) <$> many1 lower <* char ':' <*> many1 (alphaNum <|> char '#')

printResults :: [Passport] -> PuzzleAnswerPair
printResults passports = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . length . filter validPassport $ passports
    part2 = "not implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse passportsP printResults <$> getProblemInputAsByteString 4
