{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Advent2020.Day14
  ( solve
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Numeric.Natural (Natural)
import Data.Bits ((.|.), (.&.), testBit)
import Control.Monad (foldM)
import Control.Monad.Reader (MonadReader, runReader, asks)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (string)
import Text.Parsec ((<|>), try)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf, natural, word, integerWithOptionalLeadingSign)
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

inputParser :: Parser [Instruction]
inputParser = linesOf instruction 
  where
    instruction = try maskAssignment <|> writeToAddress
    maskAssignment = SetMask <$> (string "mask = " *> (Mask <$> word))
    writeToAddress = Write <$> (string "mem[" *> (Address <$> natural)) <*> (string "] = " *> integerWithOptionalLeadingSign)

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

executeInstruction :: ProgramConfig m => MachineState -> Instruction -> m MachineState
executeInstruction state (SetMask mask) = pure state{getMask=mask}
executeInstruction state@MachineState{getMemory=memory,getMask=mask} (Write (Address addrSeed) val) = do
  transformValue <- asks getValueTransformer
  decodeAddress <- asks getAddressDecoder
  let memoryValue = transformValue mask val
  let addresses = decodeAddress mask . toInteger $ addrSeed
  let newMemory = foldr (`Map.insert` memoryValue) memory addresses
  pure state{getMemory=newMemory}

-- | Returns the sum of values in memory
executeProgram :: [Instruction] -> ProgramBehavior -> Integer
executeProgram program behavior = Map.foldr (+) 0 memory
  where
    initialState = MachineState Map.empty $ Mask ""
    actionInContext = foldM executeInstruction initialState program
    MachineState{getMemory=memory} = runReader actionInContext behavior

printResults :: [Instruction] -> PuzzleAnswerPair
printResults program = PuzzleAnswerPair (part1, part2)
  where
    part1 = show $ executeProgram program . ProgramBehavior applyMaskV1 $ const pure
    part2 = show $ executeProgram program . ProgramBehavior (const id) $ applyMaskV2

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 14
