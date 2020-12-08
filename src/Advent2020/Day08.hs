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
type Program = Seq Instruction
data MachineState = MachineState { acc :: Int, pc :: Int }
data FinalState = Terminated | LoopsForever

inputParser :: Parser Program
inputParser = Seq.fromList <$> linesOf (instruction "acc" Acc <|> instruction "jmp" Jmp <|> instruction "nop" Nop)
  where
    instruction name f = f <$> ((string name *> space) *> integer)

runMachine :: Program -> (FinalState, MachineState)
runMachine program = go (MachineState 0 0) Set.empty
  where
    go :: MachineState -> Set Int -> (FinalState, MachineState)
    go state seen | pc state `Set.member` seen = (LoopsForever, state)
                  | pc state == Seq.length program = (Terminated, state)
                  | otherwise = go newState . Set.insert (pc state) $ seen
      where
        newState :: MachineState
        newState = case program !? pc state of
                     Just (Acc x) -> state { acc = acc state + x, pc = pc state + 1 }
                     Just (Jmp x) -> state { pc = pc state + x }
                     Just _ -> state { pc = pc state + 1 }

runMachine' :: Program -> [(FinalState, MachineState)]
runMachine' program = go program (MachineState 0 0) Set.empty False
  where
    go :: Program -> MachineState -> Set Int -> Bool -> [(FinalState, MachineState)]
    go p state seen flipped | pc state `Set.member` seen = [(LoopsForever, state)]
                            | pc state == Seq.length p = [(Terminated, state)]
                            | otherwise =
                                case p !? pc state of
                                  Just (Acc x) -> go p (newState (Acc x)) (Set.insert (pc state) seen) flipped
                                  Just original ->
                                    if flipped
                                    then go p (newState original) (Set.insert (pc state) seen) flipped
                                    else do
                                      doFlip <- [True, False]
                                      instruction <- if doFlip
                                                     then
                                                       case original of
                                                         Jmp x -> [Jmp x, Nop x]
                                                         Nop x -> [Jmp x, Nop x]
                                                     else
                                                       pure original
                                      let s = newState instruction
                                      let newP = adjust' (const instruction) (pc state) p
                                      go newP s (Set.insert (pc state) seen) $ flipped || doFlip
      where
        newState :: Instruction -> MachineState
        newState instruction =
          case instruction of
            Acc x -> state { acc = acc state + x, pc = pc state + 1 }
            Jmp x -> state { pc = pc state + x }
            _ -> state { pc = pc state + 1 }

fixProgram :: Program -> Maybe Int
fixProgram = go . runMachine'
  where
    go [] = Nothing
    go ((Terminated, s):_) = Just . acc $ s
    go (_:xs) = go xs

printResults :: Program -> PuzzleAnswerPair
printResults program = PuzzleAnswerPair (part1, part2)
  where
    part1 = case runMachine program of
              (LoopsForever, state) -> show . acc $ state
              (Terminated, _) -> error "unexpectedly terminated"
    part2 = case fixProgram program of
              Just acc -> show acc
              Nothing -> error "no fix found"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 8
