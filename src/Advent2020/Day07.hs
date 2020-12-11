module Advent2020.Day07
  ( solve
  ) where

import Numeric.Natural (Natural)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (find)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, string, space)
import Text.Parsec (sepBy1, (<|>), try, option)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (natural, word, linesOf)

type Color = String
data Rule = Rule { parent :: Color, children :: [(Natural, Color)] } deriving Show

inputParser :: Parser [Rule]
inputParser = linesOf rule
  where
    token :: Parser String -> Parser String
    token p = p <* space
    rule = Rule <$> color <* token bagOrBags <* token (string "contain") <*> requiredContents <* char '.'
    requiredContents = try noBags <|> sepBy1 child (string ", ")
    noBags = [] <$ string "no other bags"
    color :: Parser String
    color = (\a b -> a ++ " " ++ b) <$> token word <*> token word
    bagOrBags = string "bag" <* option ' ' (char 's')
    child = (,) <$> natural <* space <*> color <* bagOrBags

allowedContainersFor :: [Rule] -> Color -> Set Color
allowedContainersFor rules = Set.fromList . go
  where
    go y = do
      p <- map parent . filter (any (\(_, c) -> c == y) . children) $ rules
      p : go p

countChildren :: [Rule] -> Color -> Natural
countChildren rules x = maybe 0 count rule
  where
    rule = find ((==) x . parent) rules
    count = sum . map (\(k, y) -> k * (1 + countChildren rules y)) . children

printResults :: [Rule] -> PuzzleAnswerPair
printResults rules = PuzzleAnswerPair (part1, part2)
  where
    myBag = "shiny gold"
    part1 = show . Set.size . allowedContainersFor rules $ myBag
    part2 = show $ countChildren rules myBag

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 7
