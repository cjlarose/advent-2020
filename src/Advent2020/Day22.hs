{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day22
  ( solve
  ) where

import Data.Sequence (Seq((:<|)), (|>))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Text.Megaparsec (some, eof)
import Text.Megaparsec.Char.Lexer (decimal)

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse, token, symbol)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

newtype Card = Card Int deriving (Show, Eq, Ord)
type Deck = Seq Card
type CombatDecks = (Deck, Deck)

inputParser :: Parser CombatDecks
inputParser = (,) <$> deck "1" <*> deck "2" <* eof
  where
    deck playerId = symbol "Player" *> symbol playerId *> symbol ":" *> cards
    cards = Seq.fromList <$> some card
    card = Card <$> token decimal

playGame :: CombatDecks -> Deck
playGame = go
  where
    go :: CombatDecks -> Deck
    go (Seq.Empty, p2Deck) = p2Deck
    go (p1Deck, Seq.Empty) = p1Deck
    go (topP1 :<| restP1, topP2 :<| restP2)
      = if topP1 > topP2
        then go (restP1 |> topP1 |> topP2, restP2)
        else go (restP1, restP2 |> topP2 |> topP1)

getDeckScore :: Deck -> Int
getDeckScore deck = sum . zipWith (*) [length deck, length deck - 1..0] . map (\(Card x) -> x) . toList $ deck

printResults :: CombatDecks -> PuzzleAnswerPair
printResults decks = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . getDeckScore . playGame $ decks
    part2 = "not implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 22
