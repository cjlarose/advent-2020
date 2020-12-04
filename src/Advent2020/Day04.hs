module Advent2020.Day04
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (isJust)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (string, lower, char, alphaNum, endOfLine, digit, hexDigit)
import Text.Parsec (many1, sepBy1, sepEndBy1, eof, (<|>), count, try)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf, nonNegativeInteger)

data Field = BYR
           | IYR
           | EYR
           | HGT
           | HCL
           | ECL
           | PID
           | CID deriving Show
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

    byr :: Parser (String, Maybe Field)
    byr = (\a b -> (a, b)) <$> try (string "byr" <* char ':') <*> (try (validate <$> nonNegativeInteger <* endF) <|> junk)
      where validate yr | yr >= 1920 && yr <= 2002 = Just BYR
                        | otherwise = Nothing

    iyr :: Parser (String, Maybe Field)
    iyr = (\a b -> (a, b)) <$> try (string "iyr" <* char ':') <*> (try (validate <$> nonNegativeInteger <* endF) <|> junk)
      where validate yr | yr >= 2010 && yr <= 2020 = Just IYR
                        | otherwise = Nothing

    eyr :: Parser (String, Maybe Field)
    eyr = (\a b -> (a, b)) <$> try (string "eyr" <* char ':') <*> (try (validate <$> nonNegativeInteger <* endF) <|> junk)
      where validate yr | yr >= 2020 && yr <= 2030 = Just EYR
                        | otherwise = Nothing

    hgt :: Parser (String, Maybe Field)
    hgt = (\a b -> (a, b)) <$> try (string "hgt" <* char ':') <*> (try (validate <$> nonNegativeInteger <*> (string "in" <|> string "cm") <* endF) <|> junk)
      where validate cm "cm" | cm >= 150 && cm <= 193 = Just HGT
                             | otherwise = Nothing
            validate inches "in" | inches >= 59 && inches <= 76 = Just HGT
                                 | otherwise = Nothing

    hcl :: Parser (String, Maybe Field)
    hcl = (\a b -> (a, b)) <$> try (string "hcl" <* char ':') <*> (try (Just HCL <$ (char '#' *> count 6 hexDigit <* endF)) <|> junk)

    ecl :: Parser (String, Maybe Field)
    ecl = (\a b -> (a, b)) <$> try (string "ecl" <* char ':') <*> (try (Just ECL <$ validHairColor <* endF) <|> junk)
      where validHairColor = string "amb" <|> try (string "blu") <|> string "brn" <|> try (string "gry") <|> string "grn" <|> string "hzl" <|> string "oth"

    pid :: Parser (String, Maybe Field)
    pid = (\a b -> (a, b)) <$> try (string "pid" <* char ':') <*> (try (Just PID <$ count 9 digit <* endF) <|> junk)

    cid :: Parser (String, Maybe Field)
    cid = (\a b -> (a, b)) <$> try (string "cid" <* char ':') <*> (Just CID <$ many1 (alphaNum <|> char '#') <* endF)

    field :: Parser (String, Maybe Field)
    field = byr <|> iyr <|> eyr <|> hgt <|> hcl <|> ecl <|> pid <|> cid

printResults :: [Passport] -> PuzzleAnswerPair
printResults passports = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . length . filter hasRequiredKeys $ passports
    part2 = show . length . filter validPassport $ passports

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse passportsP printResults <$> getProblemInputAsByteString 4
