module Advent2020.Day08
  ( solve
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (!?), adjust')
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (string, space)
import Text.Parsec ((<|>))

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integer, linesOf)

data Instruction = Acc Int | Jmp Int | Nop Int deriving Show
data FinalState = Terminated Int | LoopsForever Int
type Program = Seq Instruction

inputParser :: Parser Program
inputParser = Seq.fromList <$> linesOf (instruction "acc" Acc <|> instruction "jmp" Jmp <|> instruction "nop" Nop)
  where
    instruction name f = f <$> ((string name *> space) *> integer)

runMachine :: Program -> FinalState
runMachine program = go 0 0 Set.empty
  where
    go :: Int -> Int -> Set Int -> FinalState
    go acc pc seen | pc `Set.member` seen = LoopsForever acc
                   | pc == Seq.length program = Terminated acc
                   | otherwise = go newAcc newPc (Set.insert pc seen)
      where
        (newAcc, newPc) = case program !? pc of
                            Just (Acc x) -> (acc + x, pc + 1)
                            Just (Jmp x) -> (acc, pc + x)
                            Just _ -> (acc, pc + 1)

fixProgram :: Program -> Maybe Int
fixProgram program = go 0
  where
    go i | i >= Seq.length program = Nothing
         | otherwise = case runMachine $ adjust' swap i program of
                         LoopsForever _ -> go (succ i)
                         Terminated acc ->  Just acc
    swap (Nop x) = Jmp x
    swap (Jmp x) = Nop x
    swap x = x

printResults :: Program -> PuzzleAnswerPair
printResults program = PuzzleAnswerPair (part1, part2)
  where
    part1 = case runMachine program of
              LoopsForever acc -> show acc
              Terminated _ -> error "unexpectedly terminated"
    part2 = case fixProgram program of
              Just acc -> show acc
              Nothing -> error "no fix found"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 8
