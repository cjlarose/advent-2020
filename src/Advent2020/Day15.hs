module Advent2020.Day15
  ( solve
  ) where

import Control.Monad (foldM)
import Control.Monad.Loops (iterateUntilM)
import Control.Monad.ST (runST)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Vector.Unboxed.Mutable (MVector, Unbox)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, endOfLine)
import Text.Parsec (sepBy1, eof)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integerWithOptionalLeadingSign)

inputParser :: Parser [Int]
inputParser = sepBy1 integerWithOptionalLeadingSign (char ',') <* endOfLine <* eof

writeSafely :: (PrimMonad m) => MVector (PrimState m) Int -> Int -> Int -> m (MVector (PrimState m) Int)
writeSafely vector i val = do
  newVec <- iterateUntilM (\v -> i < V.length v)
              (\v -> do
                let oldLength = V.length v
                new <- V.grow v oldLength
                let newLength = V.length new
                mapM_ (\i -> V.write new i (-1)) [oldLength..newLength-1]
                pure new)
              vector
  V.write newVec i val
  pure newVec

recordLastSeenIndex :: (PrimMonad m) => MVector (PrimState m) Int -> Int -> Int -> m (MVector (PrimState m) Int)
recordLastSeenIndex vector element lastIndex = writeSafely vector element lastIndex

totalRead :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> Int -> m (Maybe a)
totalRead vector i =
  if i >= V.length vector
  then pure Nothing
  else Just <$> V.read vector i

lastIndexOf :: (PrimMonad m) => MVector (PrimState m) Int -> Int -> m (Maybe Int)
lastIndexOf vector i = parseValue <$> totalRead vector i
  where
    parseValue Nothing = Nothing
    parseValue (Just (-1)) = Nothing
    parseValue (Just index) = Just index

spokenAt :: Int -> [Int] -> Int
spokenAt k inits = runST $ do
  let initialElements :: [(Int, Int)]
      initialElements = zip (init inits) [0..]
  start <- V.replicate 8 (-1)
  mostRecentIndex <- foldM (\vec (element, index) -> recordLastSeenIndex vec element index) start initialElements
  let f (i, prev, vec) = do oldVal <- lastIndexOf vec prev
                            newVec <- recordLastSeenIndex vec prev (i - 1)
                            let next = case oldVal of
                                         Just j -> i - j - 1
                                         Nothing -> 0
                            pure (i + 1, next, newVec)
  (_, last, _) <- iterateUntilM (\(i, _, _) -> i == k) f (length inits, last inits, mostRecentIndex)
  pure last

printResults :: [Int] -> PuzzleAnswerPair
printResults starting = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . spokenAt 2020 $ starting
    part2 = show . spokenAt 30000000 $ starting

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 15
