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

inputParser :: Parser [Instruction]
inputParser = linesOf instruction
  where
    token :: Parser String -> Parser String
    token p = p <* space
    mnemonic = token . string
    instruction :: Parser Instruction
    instruction = (Acc <$> (mnemonic "acc" *> integer)) <|> (Jmp <$> (mnemonic "jmp" *> integer)) <|> (Nop <$> (mnemonic "nop" *> integer))

runMachine :: [Instruction] -> Either Int Int
runMachine program = go 0 0 Set.empty
  where
    go :: Int -> Int -> Set Int -> Either Int Int
    go acc pc seen | pc `Set.member` seen = Left acc
                   | pc == length program = Right acc
                   | otherwise = go newAcc newPc (Set.insert pc seen)
      where
        (newAcc, newPc) = case program !! pc of
                             Acc x -> (acc + x, pc + 1)
                             Jmp x -> (acc, pc + x)
                             _ -> (acc, pc + 1)


modifyProgram :: [Instruction] -> Int -> [Instruction]
modifyProgram xs i = case xs !! i of
                       Acc _ -> xs
                       Jmp x -> replace i (Nop x) xs
                       Nop x -> replace i (Jmp x) xs
  where
    replace i x xs = take i xs ++ [x] ++ drop (i + 1) xs

fixProgram :: [Instruction] -> Int
fixProgram program = go 0
  where
    go i = case runMachine $ modifyProgram program i of
             Left _ -> go (succ i)
             Right acc ->  acc

printResults :: [Instruction] -> PuzzleAnswerPair
printResults instructions = PuzzleAnswerPair (part1, part2)
  where
    part1 = case runMachine instructions of
              Left acc -> show acc
              _ -> error "did not terminate"
    part2 = show . fixProgram $ instructions

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 8
