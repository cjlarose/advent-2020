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

inputParser :: Parser [Food]
inputParser = some food <* eof
  where
    food = (\is as -> Food (Set.fromList is) (Set.fromList as)) <$> some ingredient <*> parens allergens
    ingredient = Ingredient <$> token lowerWord
    allergen = Allergen <$> token lowerWord
    lowerWord = some lowerChar 
    allergens = symbol "contains" *> sepBy1 allergen (symbol ",")

-- mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
-- trh fvjkl sbzzf mxmxvkd (contains dairy)
-- sqjhc fvjkl (contains soy)
-- sqjhc mxmxvkd sbzzf (contains fish)
--
-- allergen => candidates
--
-- after reading first
-- diary => mxmxvkd kfcds sqjhc nhms 
-- fish => mxmxvkd kfcds sqjhc nhms 
--
-- second
-- diary => mxmxvkd
-- fish => mxmxvkd kfcds sqjhc nhms 
--
-- third
-- diary => mxmxvkd
-- fish => mxmxvkd kfcds sqjhc nhms 
-- soy => sqjhc fvjkl
--
-- fourth
-- diary => mxmxvkd
-- fish => mxmxvkd sqjhc
-- soy => sqjhc fvjkl
--
-- naked single
-- mxmxvkd has dairy
-- fish => sqjhc
-- soy => sqjhc fvjkl
--
-- naked single
-- mxmxvkd has dairy
-- sqjhc has fish
-- soy => fvjkl
--
-- naked single
-- mxmxvkd has dairy
-- sqjhc has fish
-- fvjkl has soy
--

numOccurencesOfNonAllergenIngredients :: [Food] -> (Int, [Ingredient])
numOccurencesOfNonAllergenIngredients foods = (totalOccurences, knownBadIngredientsSortedByAllergen)
  where
    insertFood :: Food -> CandidateMap -> CandidateMap
    insertFood Food{getIngredients=ingredients,getKnownAllergens=allergens} m
      = foldr (\allergen -> Map.insertWith Set.intersection allergen ingredients) m allergens
    candidatesForAllergens :: CandidateMap
    candidatesForAllergens = foldr insertFood Map.empty foods
    allAssigned = all Set.null . Map.elems . snd
    findNakedSingle :: (Map Allergen Ingredient, CandidateMap) -> (Map Allergen Ingredient, CandidateMap)
    findNakedSingle (known, m) = let (allergen, candidates) = Map.findMin . Map.filter ((== 1) . Set.size) $ m
                                     ingredient = Set.findMin candidates
                                     newM = Map.map (Set.delete ingredient) m
                                     newKnown = Map.insert allergen ingredient known
                                 in (newKnown, newM)
    (finalAssignments, _) = until allAssigned findNakedSingle (Map.empty, candidatesForAllergens)
    knownBadIngredients = Set.fromList . Map.elems $ finalAssignments
    allIngredients = Set.unions . map getIngredients $ foods
    nonAllergenIngredients = allIngredients \\ knownBadIngredients
    totalOccurences = sum . map (Set.size . Set.intersection nonAllergenIngredients . getIngredients) $ foods
    knownBadIngredientsSortedByAllergen = map snd . Map.toList $ finalAssignments

printResults :: [Food] -> PuzzleAnswerPair
printResults foods = PuzzleAnswerPair (part1, part2)
  where
    (occurences, knownDangerous) = numOccurencesOfNonAllergenIngredients foods
    part1 = show occurences
    part2 = intercalate "," . map (\(Ingredient x) -> x) $ knownDangerous

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 21
