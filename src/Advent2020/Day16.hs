module Advent2020.Day16
  ( solve
  ) where

import Control.Monad (guard)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, endOfLine, satisfy, string)
import Text.Parsec (sepBy1, sepEndBy1, eof, many1)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integerWithOptionalLeadingSign)

data Rule = Rule { getFieldName :: String
                 , getValidRanges :: [(Int, Int)]
                 } deriving Show
newtype Ticket = Ticket [Int] deriving Show
data ProblemInput = ProblemInput { getRules :: [Rule]
                                 , getMyTicket :: Ticket
                                 , getNearbyTickets :: [Ticket]
                                 } deriving Show

inputParser :: Parser ProblemInput
inputParser = ProblemInput <$> (rules <* endOfLine) <*> (myTicket <* endOfLine <* endOfLine) <*> nearbyTickets <* eof
  where
    rules = sepEndBy1 rule endOfLine
    rule = Rule <$> many1 (satisfy (\c -> c /= ':' && c /= '\n')) <* string ": " <*> sepBy1 range (string " or ")
    range = (,) <$> integerWithOptionalLeadingSign <* char '-' <*> integerWithOptionalLeadingSign
    myTicket = string "your ticket:" *> endOfLine *> ticket
    ticket = Ticket <$> sepBy1 integerWithOptionalLeadingSign (char ',')
    nearbyTickets = string "nearby tickets:" *> endOfLine *> sepEndBy1 ticket endOfLine

inAnyRuleRange :: [Rule] -> Int -> Bool
inAnyRuleRange rules x = any inRuleRange rules
  where
    inRuleRange Rule{getValidRanges=xs} = any inRange xs
    inRange (lo, hi) = x >= lo && x <= hi

ticketScanningErrorRate :: ProblemInput -> Int
ticketScanningErrorRate input = sum $ do
  (Ticket ticket) <- getNearbyTickets input
  fieldValue <- ticket
  guard . not . inAnyRuleRange (getRules input) $ fieldValue 
  pure fieldValue

printResults :: ProblemInput -> PuzzleAnswerPair
printResults input = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . ticketScanningErrorRate $ input
    part2 = "not implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 16
