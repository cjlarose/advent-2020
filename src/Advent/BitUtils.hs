module Advent.BitUtils
  ((<<|)
  , fromBits
  ) where

import Data.Bits (Bits, (.|.), shiftL, zeroBits, setBit)

-- | a <<| b shfits a by one bit position, then applies a bitwise OR with b
(<<|) :: Bits a => a -> a -> a
(<<|) x y = shiftL x 1 .|. y

fromBits :: (Foldable t, Bits b) => t Bool -> b
fromBits = foldl f zeroBits
  where
    f acc True = setBit (shiftL acc 1) 0
    f acc False = shiftL acc 1
