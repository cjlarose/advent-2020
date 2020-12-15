module Advent.BitUtils
  ( fromBits
  ) where

import Data.Foldable (foldl')
import Data.Bits (Bits, shiftL, zeroBits, setBit)

fromBits :: (Foldable t, Bits b) => t Bool -> b
fromBits = foldl' f zeroBits
  where
    f acc True = setBit (shiftL acc 1) 0
    f acc False = shiftL acc 1
