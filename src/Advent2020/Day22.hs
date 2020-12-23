{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day22
  ( solve
  ) where

import Data.Sequence (Seq((:<|)), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (toList)
import Text.Megaparsec (some, eof)
import Text.Megaparsec.Char.Lexer (decimal)

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse, token, symbol)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

newtype Card = Card Int deriving (Show, Eq, Ord)
type Deck = Seq Card
type CombatDecks = (Deck, Deck)
data Player = P1 | P2

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

playGameRecursive :: CombatDecks -> Deck
playGameRecursive decks = winningDeck
  where
    playRound :: Set CombatDecks -> CombatDecks -> (Player, Deck)
    playRound _ (Seq.Empty, p2Deck) = (P2, p2Deck)
    playRound _ (p1Deck, Seq.Empty) = (P1, p1Deck)
    playRound visited decks@(p1Deck,_)
      | decks `Set.member` visited = (P1, p1Deck)
      | otherwise = let (topP1 :<| restP1, topP2 :<| restP2) = decks
                        valueOf (Card x) = x
                        canRecurse = Seq.length restP1 >= valueOf topP1 && Seq.length restP2 >= valueOf topP2
                        newVisited = Set.insert decks visited
                        winner | canRecurse = fst . playRound Set.empty $ (Seq.take (valueOf topP1) restP1, Seq.take (valueOf topP2) restP2)
                               | topP1 > topP2 = P1
                               | otherwise = P2
                    in case winner of
                        P1 -> playRound newVisited (restP1 |> topP1 |> topP2, restP2)
                        P2 -> playRound newVisited (restP1, restP2 |> topP2 |> topP1)
    (_, winningDeck) = playRound Set.empty decks

getDeckScore :: Deck -> Int
getDeckScore deck = sum . zipWith (*) [length deck, length deck - 1..0] . map (\(Card x) -> x) . toList $ deck

printResults :: CombatDecks -> PuzzleAnswerPair
printResults decks = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . getDeckScore . playGame $ decks
    part2 = show . getDeckScore . playGameRecursive $ decks

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 22
