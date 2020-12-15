module Advent2020.Day15
  ( solve
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import qualified Data.HashTable.ST.Cuckoo as Cuckoo
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, endOfLine)
import Text.Parsec (sepBy1, eof)

import Advent.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.CommonParsers (integerWithOptionalLeadingSign)

inputParser :: Parser [Int]
inputParser = sepBy1 integerWithOptionalLeadingSign (char ',') <* endOfLine <* eof

spokenAt :: Int -> [Int] -> Int
spokenAt k inits = runST $ do
  indices <- Cuckoo.new
  forM_ (zip [0..] inits) $ \(i, x) -> do
    Cuckoo.insert indices x [i]
  let go last i
        | i == k = pure last
        | otherwise = do
            prevs <- Cuckoo.lookup indices last
            let next = case prevs of
                         Just (j:k:_) -> j - k
                         _ -> 0
            current <- Cuckoo.lookup indices next
            case current of
              Just (x:_) -> Cuckoo.insert indices next [i, x]
              _ -> Cuckoo.insert indices next [i]
            go next (i + 1)
  go (last inits) (length inits)

printResults :: [Int] -> PuzzleAnswerPair
printResults starting = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . spokenAt 2020 $ starting
    part2 = show . spokenAt 30000000 $ starting

solve :: IO (Either String PuzzleAnswerPair)
solve = withSuccessfulParse inputParser printResults <$> getProblemInputAsByteString 15
