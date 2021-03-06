{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Advent2020.Day25
  ( solve
  ) where

import Numeric.Natural (Natural)
import Data.Maybe (fromJust)
import Math.NumberTheory.Moduli.Class (Mod, (^%), getNatVal)
import Math.NumberTheory.Moduli.Singleton (CyclicGroup, cyclicGroup)
import Math.NumberTheory.Moduli.Multiplicative (isMultElement, isPrimitiveRoot, discreteLogarithm)
import Text.Megaparsec (eof)
import Text.Megaparsec.Char.Lexer (decimal)

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse, token)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

type PublicKey = Mod 20201227

inputParser :: Parser (PublicKey, PublicKey)
inputParser = (,) <$> publicKey <*> publicKey <* eof
  where
    publicKey = token decimal

-- | transformSubjectNumber subject loopSize = subject^loopSize mod p
transformSubjectNumber :: Mod 20201227 -> Natural -> Natural
transformSubjectNumber subject loopSize = getNatVal $ subject ^% loopSize

-- | getLoopSize pk returns the least positive integer x such that 7^x = pk
-- (mod 20201227)
getLoopSize :: PublicKey -> Natural
getLoopSize pk = fromJust $ do
  group <- cyclicGroup :: Maybe (CyclicGroup Integer 20201227)
  root <- isPrimitiveRoot group 7
  x <- isMultElement pk
  pure $ discreteLogarithm group root x

encryptionKey :: (PublicKey, PublicKey) -> Natural
encryptionKey (cardPk, doorPk) = transformSubjectNumber doorPk . getLoopSize $ cardPk

printResults :: (PublicKey, PublicKey) -> PuzzleAnswerPair
printResults publicKeys = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . encryptionKey $ publicKeys
    part2 = "not implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 25
