module Advent2020.Day08
  ( solve
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (!?), adjust')
import Data.List (iterate', find)
import Data.Maybe (listToMaybe, fromJust)
import Control.Monad.Loops (iterateUntilM)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (string, space)
import Text.Parsec ((<|>))

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integer, linesOf)

data Instruction = Acc Int | Jmp Int | Nop Int deriving Show
type Program = Seq Instruction
data MachineState = MachineState { acc :: Int, pc :: Int }
data Status = Running | TerminatedBeforeInfiniteLoop | Done deriving (Eq)

inputParser :: Parser Program
inputParser = Seq.fromList <$> linesOf (instruction "acc" Acc <|> instruction "jmp" Jmp <|> instruction "nop" Nop)
  where
    instruction name f = f <$> ((string name *> space) *> integer)

executeInstruction :: Instruction -> MachineState -> MachineState
executeInstruction instruction state =
  case instruction of
    Acc x -> state { acc = acc state + x, pc = pc state + 1 }
    Jmp x -> state { pc = pc state + x }
    _ -> state { pc = pc state + 1 }

runMachine :: Program -> Maybe MachineState
runMachine program = (\(_, st, _) -> st) <$> (find aboutToLoop . iterate' advance $ init)
  where
    aboutToLoop (TerminatedBeforeInfiniteLoop, _, _) = True
    aboutToLoop _ = False

    init = (Running, MachineState 0 0, Set.empty)
    advance (status, state, seen)
      | pc state `Set.member` seen = (TerminatedBeforeInfiniteLoop, state, seen)
      | pc state == length program = (Done, state, seen)
      | otherwise = (status, executeInstruction (fromJust $ program !? pc state) state, Set.insert (pc state) seen)

runMachine' :: Program -> [MachineState]
runMachine' program = map (\(Done, _, st, _, _) -> st) $ iterateUntilM terminated advance init
  where
    terminated :: (Status, Program, MachineState, Set Int, Bool) -> Bool
    terminated (Done, _, _, _, _) = True
    terminated _ = False

    init = (Running, program, MachineState 0 0, Set.empty, False)

    advance :: (Status, Program, MachineState, Set Int, Bool) -> [(Status, Program, MachineState, Set Int, Bool)]
    advance m@(ss, p, st, sn, flp)
      | ss == Done = [m]
      | pc st `Set.member` sn = []
      | pc st == length p = [(Done, p, st, sn, flp)]
      | flp = case p !? pc st of
                Just inst -> [(Running, p, executeInstruction inst st, Set.insert (pc st) sn, flp)]
      | otherwise = case p !? pc st of
                      Just inst@(Acc _) -> [(Running, p, executeInstruction inst st, Set.insert (pc st) sn, flp)]
                      Just original -> do
                        doFlip <- [True, False]
                        if doFlip
                        then do
                          let adjusted (Jmp x) = Nop x
                              adjusted (Nop x) = Jmp x
                          let newP = adjust' adjusted (pc st) p
                          [(Running, p, st, sn, True), (Running, newP, st, sn, True)]
                        else
                          [(Running, p, executeInstruction original st, Set.insert (pc st) sn, False)]


fixProgram :: Program -> Maybe Int
fixProgram = listToMaybe . map acc . runMachine'

printResults :: Program -> PuzzleAnswerPair
printResults program = PuzzleAnswerPair (part1, part2)
  where
    part1 = case runMachine program of
              Just state -> show . acc $ state
              Nothing -> error "unexpectedly terminated"
    part2 = case fixProgram program of
              Just acc -> show acc
              Nothing -> error "no fix found"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 8
