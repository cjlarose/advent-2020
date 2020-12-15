module Advent.BitUtils
  ((<<|)
  ) where

import Data.Bits (Bits, (.|.), shiftL)

-- | a <<| b shfits a by one bit position, then applies a bitwise OR with b
(<<|) :: Bits a => a -> a -> a
(<<|) x y = shiftL x 1 .|. y
