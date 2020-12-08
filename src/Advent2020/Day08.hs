module Advent2020.Day08
  ( solve
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (find)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, string, space)
import Text.Parsec (sepBy1, (<|>), try, option)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integer, word, linesOf)

data Instruction = Acc Int | Jmp Int | Nop deriving Show

inputParser :: Parser [Instruction]
inputParser = linesOf instruction
  where
    token :: Parser String -> Parser String
    token p = p <* space
    mnemonic = token . string
    instruction :: Parser Instruction
    instruction = (Acc <$> (mnemonic "acc" *> integer)) <|> (Jmp <$> (mnemonic "jmp" *> integer)) <|> (Nop <$ (mnemonic "nop" <* integer))

accValueBeforeLoop :: [Instruction] -> Int
accValueBeforeLoop program = go 0 0 Set.empty
  where
    go acc pc seen = if pc `Set.member` seen
                     then acc
                     else go newAcc newPc (Set.insert pc seen)
      where
        (newAcc, newPc) = case program !! pc of
                             Acc x -> (acc + x, pc + 1)
                             Jmp x -> (acc, pc + x)
                             _ -> (acc, pc + 1)

printResults :: [Instruction] -> PuzzleAnswerPair
printResults instructions = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . accValueBeforeLoop $ instructions
    part2 = "not implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 8
