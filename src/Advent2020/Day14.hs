module Advent2020.Day14
  ( solve
  ) where

import Numeric.Natural (Natural)
import Data.List (foldl')
import Data.Bits ((.|.), (.&.), shiftL, testBit)
import Control.Monad (foldM_, forM_, foldM)
import Control.Monad.ST (runST)
import qualified Data.HashTable.ST.Cuckoo as Cuckoo
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (string)
import Text.Parsec ((<|>), try)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf, natural, word, integerWithOptionalLeadingSign)

newtype Address = Address Natural deriving Show
data MaskV1 = MaskV1 { getClearingMask :: Integer, getSettingMask :: Integer } deriving Show
newtype Mask = Mask String deriving Show
data Instruction = SetMask Mask | Write Address Integer deriving Show

inputParser :: Parser [Instruction]
inputParser = linesOf instruction 
  where
    instruction = try maskAssignment <|> writeToAddress
    maskAssignment = SetMask <$> (string "mask = " *> (Mask <$> word))
    writeToAddress = Write <$> (string "mem[" *> (Address <$> natural)) <*> (string "] = " *> integerWithOptionalLeadingSign)

-- | Constructs a version 1 mask given a mask in its string form. A verison 1
-- mask is composed to two parts: a clearing mask and setting mask. The
-- clearing mask has a 0 in every bit position we want to clear, 1s elsewhere.
-- The setting mask has a 1 in every position we want to set to 1, 0s
-- elsewhere.
parseVersion1Mask :: Mask -> MaskV1
parseVersion1Mask (Mask xs) = let (c, s) = foldl' f (0, 0) xs in MaskV1 { getClearingMask=c, getSettingMask=s }
  where
    f :: (Integer, Integer) -> Char -> (Integer, Integer)
    f (clearMask, setMask) '0' = (shiftL clearMask 1, shiftL setMask 1)
    f (clearMask, setMask) '1' = (shiftL clearMask 1 .|. 1, shiftL setMask 1 .|. 1)
    f (clearMask, setMask) 'X' = (shiftL clearMask 1 .|. 1, shiftL setMask 1)

applyMaskV1 :: Mask -> Integer -> Integer
applyMaskV1 mask = setBits . clearBits
  where
    maskV1 = parseVersion1Mask mask
    clearBits = (.&.) (getClearingMask maskV1)
    setBits = (.|.) (getSettingMask maskV1)

applyMaskV2 :: Mask -> Integer -> [Integer]
applyMaskV2 (Mask mask) val = do
  let f :: Integer -> (Char, Bool) -> [Integer]
      f acc ('0', v) = if v then [shiftL acc 1 .|. 1] else [shiftL acc 1]
      f acc ('1', _) = [shiftL acc 1 .|. 1]
      f acc ('X', _) = [shiftL acc 1, shiftL acc 1 .|. 1]
  foldM f 0 . zip mask . map (testBit val) $ [35,34..0]

-- | Returns the sum of values in memory
executeProgram :: [Instruction] -> Integer
executeProgram program = runST $ do
  memory <- Cuckoo.new
  let f _ (SetMask mask) = pure mask
      f mask (Write (Address addr) val) = do
        Cuckoo.insert memory (toInteger addr) . applyMaskV1 mask $ val
        pure mask
  foldM_ f (Mask "") program
  Cuckoo.foldM (\acc (_, v) -> pure $ acc + v) 0 memory

-- | Returns the sum of values in memory
executeV2Program :: [Instruction] -> Integer
executeV2Program program = runST $ do
  memory <- Cuckoo.new
  let f _ (SetMask mask) = pure mask
      f mask (Write (Address addrSeed) val) = do
        let addresses = applyMaskV2 mask . toInteger $ addrSeed
        forM_ addresses $ \addr -> do
          Cuckoo.insert memory (toInteger addr) val
        pure mask
  foldM_ f (Mask "") program
  Cuckoo.foldM (\acc (_, v) -> pure $ acc + v) 0 memory

printResults :: [Instruction] -> PuzzleAnswerPair
printResults program = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . executeProgram $ program
    part2 = show . executeV2Program $ program

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 14
