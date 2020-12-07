module Advent2020.Day07
  ( solve
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, string, space)
import Text.Parsec (sepBy1, (<|>), count, try, option)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (nonNegativeInteger, word, linesOf)

type Color = String
data Rule = Rule { parent :: Color, children :: [(Int, Color)] } deriving Show

inputParser :: Parser [Rule]
inputParser = linesOf rule
  where
    token :: Parser String -> Parser String
    token p = p <* space
    rule = (\p xs -> Rule { parent=p, children=xs }) <$> color <* token bagOrBags <* token (string "contain") <*> (try noBags <|> sepBy1 child (string ", ")) <* char '.'
    noBags = [] <$ string "no other bags"
    color :: Parser String
    color = (\a b -> a ++ " " ++ b) <$> token word <*> token word
    bagOrBags = string "bag" <* option ' ' (char 's')
    child = (\a b -> (a, b)) <$> nonNegativeInteger <* space <*> color <* bagOrBags

parentsOf :: [Rule] -> Color -> Set Color
parentsOf rules x = Set.fromList $ go x
  where
    go y = do
      parent <- map parent . filter (\Rule{ children=children } -> any (\(_, c) -> c == y) children) $ rules
      parent : go parent

countChildren :: [Rule] -> Color -> Int
countChildren rules x = sum . map (\(k, y) -> k * (1 + countChildren rules y)) $ children rule
  where
    rule = head . filter (\Rule{ parent=parent } -> parent == x) $ rules

printResults :: [Rule] -> PuzzleAnswerPair
printResults rules = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . Set.size . parentsOf rules $ "shiny gold"
    part2 = show $ countChildren rules "shiny gold"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 7