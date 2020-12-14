module Advent2020.Day14
  ( solve
  ) where

import Numeric.Natural (Natural)
import Data.List (foldl')
import Data.Bits ((.|.), (.&.), shiftL, complement)
import Control.Monad (foldM_)
import Control.Monad.ST (runST)
import qualified Data.HashTable.ST.Cuckoo as Cuckoo
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (string)
import Text.Parsec ((<|>), try)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (linesOf, natural, word, integerWithOptionalLeadingSign)

newtype Address = Address Natural deriving Show
data Mask = Mask { getClearingMask :: Integer, getSettingMask :: Integer } deriving Show
data Instruction = SetMask Mask | Write Address Integer deriving Show

-- and with 0s to clear in those spots, 1s where we want them to be unchanged
-- to set bits, execute OR

inputParser :: Parser [Instruction]
inputParser = linesOf instruction 
  where
    instruction = try maskAssignment <|> writeToAddress
    maskAssignment = SetMask <$> (string "mask = " *> (parseMask <$> word))
    writeToAddress = Write <$> (string "mem[" *> (Address <$> natural)) <*> (string "] = " *> integerWithOptionalLeadingSign)
    parseMask xs = let (c, s) = foldl' f (0, 0) xs in Mask { getClearingMask=complement c, getSettingMask=s }
    f :: (Integer, Integer) -> Char -> (Integer, Integer)
    f (clearMask, setMask) '0' = (shiftL clearMask 1 .|. 1, shiftL setMask 1)
    f (clearMask, setMask) '1' = (shiftL clearMask 1, shiftL setMask 1 .|. 1)
    f (clearMask, setMask) 'X' = (shiftL clearMask 1, shiftL setMask 1)

emptyMask :: Mask
emptyMask = Mask { getClearingMask=0, getSettingMask=0 }

applyMask :: Mask -> Integer -> Integer
applyMask mask = setBits . clearBits
  where
    clearBits = (.&.) (getClearingMask mask)
    setBits = (.|.) (getSettingMask mask)

-- | Returns the sum of values in memory
executeProgram :: [Instruction] -> Integer
executeProgram program = runST $ do
  memory <- Cuckoo.new
  let f _ (SetMask mask) = pure mask
      f mask (Write (Address addr) val) = do
        Cuckoo.insert memory (toInteger addr) . applyMask mask $ val
        pure mask
  foldM_ f emptyMask program
  Cuckoo.foldM (\acc (_, v) -> pure $ acc + v) 0 memory

printResults :: [Instruction] -> PuzzleAnswerPair
printResults program = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . executeProgram $ program
    part2 = "not yet implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 14
