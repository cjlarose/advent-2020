module Advent2020.Day16
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
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
inputParser = ProblemInput <$> rules <* endOfLine <*> myTicket <* endOfLine <*> nearbyTickets <* eof
  where
    rules = sepEndBy1 rule endOfLine
    fieldName = many1 (satisfy (\c -> c /= ':' && c /= '\n'))
    rule = Rule <$> fieldName <* string ": " <*> sepBy1 range (string " or ")
    range = (,) <$> integerWithOptionalLeadingSign <* char '-' <*> integerWithOptionalLeadingSign
    myTicket = string "your ticket:" *> endOfLine *> ticket <* endOfLine
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
    fieldHasDeparture name = elem "departure" . words $ name

getFieldOrder :: [Rule] -> [Ticket] -> [String]
getFieldOrder rules tickets = head possibleFieldOrders
  where
    numCols = length . head . map (\(Ticket t) -> t) $ tickets
    allFields = Set.fromList rules
    ticketValues pos = map (\(Ticket t) -> t !! pos) tickets
    possibleFieldsForColumn pos remaining = Set.filter validRuleForPosition remaining
      where
        validRuleForPosition rule = all (valueInRangeForRule rule) $ ticketValues pos
    possibleFields :: Map Int (Set Rule)
    possibleFields = Map.fromList . map (\pos -> (pos, possibleFieldsForColumn pos allFields)) $ [0..numCols - 1]
    possibleFieldOrders :: [[String]]
    possibleFieldOrders = orders
      where
        toOrder = map snd . Map.toAscList
        initialValues = (allFields, Map.empty, possibleFields)
        searchComplete (fields, _, _) = Set.size fields == 0
        orders = map (\(_, m, _) -> toOrder m) . iterateUntilM searchComplete f $ initialValues
        idealCandidate = minimumBy (comparing (Set.size . snd)) . Map.toList
        f (remainingFields, positionToFieldMap, candidates) = do
          let (pos, candidateFieldsForPosition) = idealCandidate candidates
          rule <- Set.toList candidateFieldsForPosition
          let newRemainingFields = Set.delete rule remainingFields
          let newPositionToFieldMap = Map.insert pos (getFieldName rule) positionToFieldMap
          let newCandidates = Map.map (Set.delete rule) . Map.delete pos $ candidates
          pure (newRemainingFields, newPositionToFieldMap, newCandidates)

myTicketCode :: ProblemInput -> Int
myTicketCode ProblemInput{getRules=rules
                         ,getMyTicket=myTicket
                         ,getNearbyTickets=nearbyTickets} = code
  where
    validTickets = filter (isValidTicket rules) nearbyTickets
    fieldOrder = getFieldOrder rules validTickets
    code = ticketCode myTicket fieldOrder

printResults :: ProblemInput -> PuzzleAnswerPair
printResults input = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . ticketScanningErrorRate $ input
    part2 = show . myTicketCode $ input

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 16
