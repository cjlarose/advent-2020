module Advent2020.Day16
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Ord (comparing)
import Control.Monad (guard)
import Control.Monad.Loops (iterateUntilM)
import Data.Foldable (minimumBy)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, endOfLine, satisfy, string)
import Text.Parsec (sepBy1, sepEndBy1, eof, many1)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integerWithOptionalLeadingSign)

data Rule = Rule { getFieldName :: String
                 , getValidRanges :: [(Int, Int)]
                 } deriving (Show, Ord, Eq)
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
inAnyRuleRange rules x = any (`valueInRangeForRule` x) rules

ticketScanningErrorRate :: ProblemInput -> Int
ticketScanningErrorRate input = sum $ do
  (Ticket ticket) <- getNearbyTickets input
  fieldValue <- ticket
  guard . not . inAnyRuleRange (getRules input) $ fieldValue 
  pure fieldValue

isValidTicket :: [Rule] -> Ticket -> Bool
isValidTicket rules (Ticket ticket) = all (inAnyRuleRange rules) ticket

valueInRangeForRule :: Rule -> Int -> Bool
valueInRangeForRule Rule{getValidRanges=xs} x = any inRange xs
  where
    inRange (lo, hi) = x >= lo && x <= hi

ticketCode :: Ticket -> [String] -> Int
ticketCode (Ticket values) = product . map fst . filter (fieldHasDeparture . snd) . zip values
  where
    fieldHasDeparture "departure location" = True
    fieldHasDeparture "departure station" = True
    fieldHasDeparture "departure platform" = True
    fieldHasDeparture "departure track" = True
    fieldHasDeparture "departure date" = True
    fieldHasDeparture "departure time" = True
    fieldHasDeparture _ = False

getFieldOrder :: [Rule] -> [Ticket] -> [String]
getFieldOrder rules tickets = possibleFieldOrder
  where
    numCols = length . head . map (\(Ticket t) -> t) $ tickets
    allFields = Set.fromList rules
    possibleFieldsForColumn pos remaining = do
      rule <- Set.toList remaining
      let ticketValues = map (\(Ticket t) -> t !! pos) tickets
      guard . all (valueInRangeForRule rule) $ ticketValues
      pure rule
    possibleFields :: Map Int (Set Rule)
    possibleFields = Map.fromList . map (\pos -> (pos, Set.fromList $ possibleFieldsForColumn pos allFields)) $ [0..numCols - 1]
    possibleFieldOrders :: [[String]]
    possibleFieldOrders = orders
      where
        toOrder = map snd . Map.toAscList
        orders = map (\(_, _, m) -> toOrder m) . iterateUntilM (\(pos, _, _) -> Set.size pos == 0) f $ (Set.fromList [0..numCols - 1], allFields, Map.empty)
        f (remainingPositions, remainingFields, positionToFieldMap) = do
          let candidates :: Map Int Int
              candidates = Map.map (\v -> Set.size $ Set.intersection v remainingFields) . Map.restrictKeys possibleFields $ remainingPositions
          guard $ Map.size candidates > 0
          let posWithMinPossibleRemainingFields :: Int
              posWithMinPossibleRemainingFields = fst . minimumBy (comparing snd) . Map.toList $ candidates
          let candidateFieldsForPosition = Set.intersection remainingFields $ possibleFields ! posWithMinPossibleRemainingFields
          let g rule = ( Set.delete posWithMinPossibleRemainingFields remainingPositions
                       , Set.delete rule remainingFields
                       , Map.insert posWithMinPossibleRemainingFields (getFieldName rule) positionToFieldMap
                       )
          map g . Set.toList $ candidateFieldsForPosition
    [possibleFieldOrder] = possibleFieldOrders

myTicketCode :: ProblemInput -> Int
myTicketCode input = code
  where
    fieldOrder = getFieldOrder (getRules input) (filter (isValidTicket (getRules input)) $ getNearbyTickets input)
    code = ticketCode (getMyTicket input) fieldOrder

printResults :: ProblemInput -> PuzzleAnswerPair
printResults input = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . ticketScanningErrorRate $ input
    part2 = show . myTicketCode $ input

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 16
