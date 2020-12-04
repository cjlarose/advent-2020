module Advent2020.Day04
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (isJust)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, string, alphaNum, endOfLine, digit, hexDigit)
import Text.Parsec (many1, sepBy1, eof, (<|>), count, try, choice)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (nonNegativeInteger)

data Field = BYR | IYR | EYR | HGT | HCL | ECL | PID | CID deriving Show
type Passport = Map String (Maybe Field)

requiredKeys :: Set String
requiredKeys = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

hasRequiredKeys :: Passport -> Bool
hasRequiredKeys = Set.isSubsetOf requiredKeys . Map.keysSet

validPassport :: Passport -> Bool
validPassport passport = hasRequiredKeys passport && allRequiredFieldsValid
  where
    allRequiredFieldsValid = all isJust . Map.elems . Map.restrictKeys passport $ requiredKeys

passportsP :: Parser [Passport]
passportsP = sepBy1 passport endOfLine <* eof
  where
    passport = Map.fromList <$> many1 field
    endF = endOfLine <|> char ' '
    junk = many1 (alphaNum <|> char '#')
    junkF = Nothing <$ junk <* endF
    fieldType key p = (\a b -> (a, b)) <$> try (string key <* char ':') <*> (try (p <* endF) <|> junkF)
    anyOf = choice . map (try . string . show)
    byr = fieldType "byr" (Just BYR <$ anyOf [1920..2002])
    iyr = fieldType "iyr" (Just IYR <$ anyOf [2010..2020])
    eyr = fieldType "eyr" (Just EYR <$ anyOf [2020..2030])
    hgt = fieldType "hgt" (Just HGT <$ (try (anyOf [150..193] <* string "cm") <|> (anyOf [59..76] <* string "in")))
    hcl = fieldType "hcl" (Just HCL <$ char '#' <* count 6 hexDigit)
    ecl = fieldType "ecl" (Just ECL <$ (choice . map (try . string) $ ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]))
    pid = fieldType "pid" (Just PID <$ count 9 digit)
    cid = fieldType "cid" (Just CID <$ junk)
    field = byr <|> iyr <|> eyr <|> hgt <|> hcl <|> ecl <|> pid <|> cid

printResults :: [Passport] -> PuzzleAnswerPair
printResults passports = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . length . filter hasRequiredKeys $ passports
    part2 = show . length . filter validPassport $ passports

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse passportsP printResults <$> getProblemInputAsByteString 4
