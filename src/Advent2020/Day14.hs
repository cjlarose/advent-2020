{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day14
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Numeric.Natural (Natural)
import Data.Bits ((.|.), (.&.), testBit)
import Control.Monad (foldM)
import Control.Monad.Reader (MonadReader, Reader, runReader, asks)
import Control.Monad.State.Strict (MonadState, StateT, gets, modify, runStateT)
import Text.Megaparsec (some, try, eof, (<|>))
import Text.Megaparsec.Char.Lexer (decimal)

import Advent.Input (getProblemInputAsText)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.Parse (Parser, parse, natural, word, token, symbol, brackets)
import Advent.BitUtils (fromBits)

newtype Address = Address Natural deriving Show
newtype Mask = Mask String deriving Show
data Instruction = SetMask Mask | Write Address Integer deriving Show
data ProgramBehavior = ProgramBehavior { getValueTransformer :: Mask -> Integer -> Integer
                                       , getAddressDecoder :: Mask -> Integer -> [Integer]
                                       }
type ProgramConfig = MonadReader ProgramBehavior
data MachineState = MachineState { getMemory :: Map Integer Integer
                                 , getMask :: Mask
                                 }

newtype VM a = VM {
  runVM :: StateT MachineState (Reader ProgramBehavior) a
} deriving (Monad, Applicative, Functor, MonadState MachineState, ProgramConfig)

inputParser :: Parser [Instruction]
inputParser = some instruction <* eof
  where
    instruction = try maskAssignment <|> writeToAddress
    maskAssignment = SetMask . Mask <$> (symbol "mask" *> symbol "=" *> token word)
    writeToAddress = Write <$> (symbol "mem" *> brackets (Address <$> natural) <* symbol "=") <*> token decimal

-- | Applies a version 1 mask given a mask in its string form. A verison 1 mask
-- is composed to two parts: a clearing mask and setting mask. The clearing
-- mask has a 0 in every bit position we want to clear, 1s elsewhere. The
-- setting mask has a 1 in every position we want to set to 1, 0s elsewhere.
applyMaskV1 :: Mask -> Integer -> Integer
applyMaskV1 (Mask mask) = setBits . clearBits
  where
    clearBits = (.&.) . fromBits . map (/= '0') $ mask
    setBits = (.|.) . fromBits . map (== '1') $ mask

applyMaskV2 :: Mask -> Integer -> [Integer]
applyMaskV2 (Mask mask) val = do
  let f :: [Bool] -> (Char, Bool) -> [[Bool]]
      f acc ('0', v) = pure $ v : acc
      f acc ('1', _) = pure $ True : acc
      f acc ('X', _) = [False : acc, True : acc]
  let bitPatterns = foldM f [] . zip (reverse mask) . map (testBit val) $ [0..]
  map fromBits bitPatterns

executeInstruction :: Instruction -> VM ()
executeInstruction (SetMask mask) = modify (\state -> state{getMask=mask})
executeInstruction (Write (Address addrSeed) val) = do
  transformValue <- asks getValueTransformer
  decodeAddress <- asks getAddressDecoder
  mask <- gets getMask
  let memoryValue = transformValue mask val
  let addresses = decodeAddress mask . toInteger $ addrSeed
  memory <- gets getMemory
  let newMemory = foldr (`Map.insert` memoryValue) memory addresses
  modify (\state -> state{getMemory=newMemory})

sumOfMemoryValues :: VM Integer
sumOfMemoryValues = Map.foldr (+) 0 <$> gets getMemory

-- | Returns the sum of values in memory
executeProgram :: [Instruction] -> ProgramBehavior -> Integer
executeProgram program behavior = memorySum
  where
    initialState = MachineState Map.empty $ Mask ""
    executeAndSum = mapM_ executeInstruction program >> sumOfMemoryValues
    (memorySum, _) = runReader (runStateT (runVM executeAndSum) initialState) behavior

printResults :: [Instruction] -> PuzzleAnswerPair
printResults program = PuzzleAnswerPair (part1, part2)
  where
    part1 = show $ executeProgram program . ProgramBehavior applyMaskV1 $ const pure
    part2 = show $ executeProgram program . ProgramBehavior (const id) $ applyMaskV2

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 14
