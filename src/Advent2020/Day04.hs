module Advent2020.Day04
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, string, alphaNum, endOfLine, digit, hexDigit)
import Text.Parsec (many1, sepBy1, eof, (<|>), count, try, choice)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (nonNegativeInteger, word)

data FieldValue = Valid | Invalid deriving Show
type Passport = Map String FieldValue

requiredKeys :: Set String
requiredKeys = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

hasRequiredKeys :: Passport -> Bool
hasRequiredKeys = Set.isSubsetOf requiredKeys . Map.keysSet

validPassport :: Passport -> Bool
validPassport passport = hasRequiredKeys passport && allRequiredFieldsValid
  where
    allRequiredFieldsValid = all isValid . Map.elems . Map.restrictKeys passport $ requiredKeys
    isValid Valid = True
    isValid _ = False

passportsP :: Parser [Passport]
passportsP = sepBy1 passport endOfLine <* eof
  where
    passport = Map.fromList <$> many1 field
    token :: Parser String -> Parser String
    token p = p <* (endOfLine <|> char ' ')
    fieldType key p = (\a b -> (a, b)) <$> try (string key <* char ':') <*> (try (Valid <$ token p) <|> (Invalid <$ token word))
    nonNegativeDecimalIntegerInRange min max = do
      val <- nonNegativeInteger
      if val >= min && val <= max
        then pure . show $ val
        else fail "no parse"
    byr = fieldType "byr" $ nonNegativeDecimalIntegerInRange 1920 2002
    iyr = fieldType "iyr" $ nonNegativeDecimalIntegerInRange 2010 2020
    eyr = fieldType "eyr" $ nonNegativeDecimalIntegerInRange 2020 2030
    hgt = fieldType "hgt" $ try (nonNegativeDecimalIntegerInRange 150 193 <* string "cm") <|> (nonNegativeDecimalIntegerInRange 59 76 <* string "in")
    hcl = fieldType "hcl" $ char '#' *> count 6 hexDigit
    ecl = fieldType "ecl" . choice . map (try . string) $ ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    pid = fieldType "pid" $ count 9 digit
    cid = fieldType "cid" word
    field = choice [byr, iyr, eyr, hgt, hcl, ecl, pid, cid]

printResults :: [Passport] -> PuzzleAnswerPair
printResults passports = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . length . filter hasRequiredKeys $ passports
    part2 = show . length . filter validPassport $ passports

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse passportsP printResults <$> getProblemInputAsByteString 4
