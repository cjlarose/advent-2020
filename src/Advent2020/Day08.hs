module Advent2020.Day08
  ( solve
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (string, space)
import Text.Parsec ((<|>))

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integer, linesOf)

data Instruction = Acc Int | Jmp Int | Nop Int deriving Show
data FinalState = Terminated Int | LoopsForever Int
type Program = [Instruction]

inputParser :: Parser Program
inputParser = linesOf $ instruction "acc" Acc <|> instruction "jmp" Jmp <|> instruction "nop" Nop
  where
    instruction name f = f <$> ((string name *> space) *> integer)

runMachine :: Program -> FinalState
runMachine program = go 0 0 Set.empty
  where
    go :: Int -> Int -> Set Int -> FinalState
    go acc pc seen | pc `Set.member` seen = LoopsForever acc
                   | pc == length program = Terminated acc
                   | otherwise = go newAcc newPc (Set.insert pc seen)
      where
        (newAcc, newPc) = case program !! pc of
                             Acc x -> (acc + x, pc + 1)
                             Jmp x -> (acc, pc + x)
                             _ -> (acc, pc + 1)


modifyProgram :: Program -> Int -> Program
modifyProgram xs i = case xs !! i of
                       Acc _ -> xs
                       Jmp x -> replace i (Nop x) xs
                       Nop x -> replace i (Jmp x) xs
  where
    replace i x xs = take i xs ++ [x] ++ drop (i + 1) xs

fixProgram :: Program -> Maybe Int
fixProgram program = go 0
  where
    go i | i >= length program = Nothing
         | otherwise = case runMachine $ modifyProgram program i of
                         LoopsForever _ -> go (succ i)
                         Terminated acc ->  Just acc

printResults :: Program -> PuzzleAnswerPair
printResults instructions = PuzzleAnswerPair (part1, part2)
  where
    part1 = case runMachine instructions of
              LoopsForever acc -> show acc
              Terminated _ -> error "unexpectedly terminated"
    part2 = case fixProgram instructions of
              Just acc -> show acc
              Nothing -> error "no fix found"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 8
