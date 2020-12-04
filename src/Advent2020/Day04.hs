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
import Text.Parsec (many1, sepBy1, eof, (<|>), count, try)

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
    passport :: Parser Passport
    passport = Map.fromList <$> many1 field

    endF = endOfLine <|> char ' '
    junk = Nothing <$ many1 (alphaNum <|> char '#') <* endF

    fieldType :: String -> Parser (Maybe Field) -> Parser (String, Maybe Field)
    fieldType key p = (\a b -> (a, b)) <$> try (string key <* char ':') <*> (try (p <* endF) <|> junk)

    byr = fieldType "byr" (validate <$> nonNegativeInteger)
      where validate yr | yr >= 1920 && yr <= 2002 = Just BYR
                        | otherwise = Nothing

    iyr = fieldType "iyr" (validate <$> nonNegativeInteger)
      where validate yr | yr >= 2010 && yr <= 2020 = Just IYR
                        | otherwise = Nothing

    eyr = fieldType "eyr" (validate <$> nonNegativeInteger)
      where validate yr | yr >= 2020 && yr <= 2030 = Just EYR
                        | otherwise = Nothing

    hgt = fieldType "hgt" (validate <$> nonNegativeInteger <*> (string "in" <|> string "cm"))
      where validate cm "cm" | cm >= 150 && cm <= 193 = Just HGT
                             | otherwise = Nothing
            validate inches "in" | inches >= 59 && inches <= 76 = Just HGT
                                 | otherwise = Nothing

    hcl = fieldType "hcl" (Just HCL <$ char '#' <* count 6 hexDigit)

    ecl = fieldType "ecl" (Just ECL <$ validHairColor)
      where validHairColor = string "amb" <|> try (string "blu") <|> string "brn" <|> try (string "gry") <|> string "grn" <|> string "hzl" <|> string "oth"

    pid = fieldType "pid" (Just PID <$ count 9 digit)

    cid = fieldType "cid" (Just CID <$ many1 (alphaNum <|> char '#'))

    field :: Parser (String, Maybe Field)
    field = byr <|> iyr <|> eyr <|> hgt <|> hcl <|> ecl <|> pid <|> cid

printResults :: [Passport] -> PuzzleAnswerPair
printResults passports = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . length . filter hasRequiredKeys $ passports
    part2 = show . length . filter validPassport $ passports

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse passportsP printResults <$> getProblemInputAsByteString 4
