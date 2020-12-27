{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day24
  ( solve
  ) where

import qualified Data.Set as Set
import Data.Set (Set, (\\))
import Text.Megaparsec (some, eof, try)
import Text.Megaparsec.Char (string)
import Control.Monad.Combinators (choice)

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse, token)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

data Direction = E | SE | SW | W | NW | NE deriving Show
type Path = [Direction]
type Vec = (Int, Int, Int)

toDisplacementVector :: Direction -> Vec
toDisplacementVector E = (1, -1, 0)
toDisplacementVector SE = (0, -1, 1)
toDisplacementVector SW = (-1, 0, 1)
toDisplacementVector W = (-1, 1, 0)
toDisplacementVector NW = (0, 1, -1)
toDisplacementVector NE = (1, 0, -1)

inputParser :: Parser [Path]
inputParser = some path <* eof
  where
    path = token . some $ direction
    direction = choice . map try $ [ dir E "e"
                                   , dir SE "se"
                                   , dir SW "sw"
                                   , dir W "w"
                                   , dir NW "nw"
                                   , dir NE "ne"
                                   ]
    dir f s = f <$ string s

vecAdd :: Vec -> Vec -> Vec
vecAdd (i, j, k) (i', j', k') = (i + i', j + j', k + k')

toCoord :: Path -> Vec
toCoord = foldr (vecAdd . toDisplacementVector) (0, 0, 0)

blackTiles :: [Path] -> Set Vec
blackTiles = foldr (f . toCoord) Set.empty
  where
    f coord acc = if coord `Set.member` acc
                  then Set.delete coord acc
                  else Set.insert coord acc

neighbors :: Vec -> Set Vec
neighbors coord = Set.fromList . map (vecAdd coord . toDisplacementVector) $ [E, SE, SW, W, NW, NE]

simulateDays :: Int -> Set Vec -> Set Vec
simulateDays 0 blackTiles = blackTiles
simulateDays i blackTiles =
  let flipsToWhite coord = (\x -> x == 0 || x > 2) . Set.size . Set.intersection blackTiles . neighbors $ coord
      flippedToWhite = Set.filter flipsToWhite blackTiles
      candidateWhiteTiles = Set.unions (Set.map neighbors blackTiles) \\ blackTiles
      turnsBlack coord = (== 2) . Set.size . Set.intersection blackTiles . neighbors $ coord
      newlyBlackTiles = Set.filter turnsBlack candidateWhiteTiles
      newBlackTiles = Set.union (blackTiles \\ flippedToWhite) newlyBlackTiles
  in simulateDays (i - 1) newBlackTiles

printResults :: [Path] -> PuzzleAnswerPair
printResults paths = PuzzleAnswerPair (part1, part2)
  where
    initialBlackTiles = blackTiles paths
    part1 = show . Set.size $ initialBlackTiles
    part2 = show . Set.size . simulateDays 100 $ initialBlackTiles

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 24
