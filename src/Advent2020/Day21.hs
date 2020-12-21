{-# LANGUAGE OverloadedStrings #-}

module Advent2020.Day21
  ( solve
  ) where

import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import Text.Megaparsec (some, eof)
import Text.Megaparsec.Char (lowerChar)
import Control.Monad.Combinators (sepBy1)

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse, token, symbol, parens)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

newtype Ingredient = Ingredient String deriving (Show, Eq, Ord)
newtype Allergen = Allergen String deriving (Show, Eq, Ord)
data Food = Food { getIngredients :: Set Ingredient
                 , getKnownAllergens :: Set Allergen
                 } deriving Show
type CandidateMap = Map Allergen (Set Ingredient)
type KnownAllergenMap = Map Allergen Ingredient

inputParser :: Parser [Food]
inputParser = some food <* eof
  where
    food = (\is as -> Food (Set.fromList is) (Set.fromList as)) <$> some ingredient <*> parens allergens
    ingredient = Ingredient <$> token lowerWord
    allergen = Allergen <$> token lowerWord
    lowerWord = some lowerChar 
    allergens = symbol "contains" *> sepBy1 allergen (symbol ",")

ingredientsByAllergen :: [Food] -> KnownAllergenMap
ingredientsByAllergen foods = finalAssignments
  where
    insertFood :: Food -> CandidateMap -> CandidateMap
    insertFood Food{getIngredients=ingredients,getKnownAllergens=allergens} m
      = foldr (\allergen -> Map.insertWith Set.intersection allergen ingredients) m allergens
    candidatesForAllergens :: CandidateMap
    candidatesForAllergens = foldr insertFood Map.empty foods
    allAssigned = all Set.null . Map.elems . snd
    findNakedSingle :: (KnownAllergenMap, CandidateMap) -> (KnownAllergenMap, CandidateMap)
    findNakedSingle (known, m) = let (allergen, candidates) = Map.findMin . Map.filter ((== 1) . Set.size) $ m
                                     ingredient = Set.findMin candidates
                                     newM = Map.map (Set.delete ingredient) m
                                     newKnown = Map.insert allergen ingredient known
                                 in (newKnown, newM)
    (finalAssignments, _) = until allAssigned findNakedSingle (Map.empty, candidatesForAllergens)

numOccurencesOfInertIngredients :: [Food] -> Int
numOccurencesOfInertIngredients foods = totalOccurences
  where
    knownDangerousIngredients = Set.fromList . Map.elems . ingredientsByAllergen $ foods
    allIngredients = Set.unions . map getIngredients $ foods
    inertIngredients = allIngredients \\ knownDangerousIngredients
    totalOccurences = sum . map (Set.size . Set.intersection inertIngredients . getIngredients) $ foods

knownDangerousIngredientsSortedByAllergen :: [Food] -> [Ingredient]
knownDangerousIngredientsSortedByAllergen = map snd . Map.toList . ingredientsByAllergen

printResults :: [Food] -> PuzzleAnswerPair
printResults foods = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . numOccurencesOfInertIngredients $ foods
    part2 = intercalate "," . map (\(Ingredient x) -> x) . knownDangerousIngredientsSortedByAllergen $ foods

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 21
