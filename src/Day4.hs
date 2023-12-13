{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day4 where

import Adventlib
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import Data.String (String)
import Protolude hiding (Map)
import Protolude.Partial (fromJust)
import Text.Regex.Applicative
import Text.Regex.Applicative.Common

puzzle1 :: [String] -> Integer
puzzle1 = sum . map (pointsForMatches . snd . matchesForCard . fromJust . match parseCard)

puzzle2 :: [String] -> Integer
puzzle2 = sum . foldl' winCards M.empty . map (matchesForCard . fromJust . match parseCard)

newtype WInt = WInt {unWInt :: Integer}
  deriving (Eq, Show, Num, Ord)

newtype OInt = OInt Integer
  deriving (Eq, Show, Num, Ord)

newtype Matches = Matches Integer
  deriving (Eq, Show, Num, Ord)

newtype Points = Points Integer
  deriving (Eq, Show, Num, Ord)

newtype CardId = CardId Integer
  deriving (Eq, Show, Num, Ord)

data Card = Card
  { _id :: !CardId,
    _winners :: ![WInt],
    _owned :: ![OInt]
  }
  deriving (Eq, Show)

pointsForMatches :: Matches -> Integer
pointsForMatches (Matches x)
  | x > 0 = 2 ^ (x - 1)
  | otherwise = 0

matchesForCard :: Card -> (CardId, Matches)
matchesForCard (Card id winners owned) =
  (id, getSum $ foldMap (isWin winners) owned)

isWin :: [WInt] -> OInt -> Sum Matches
isWin winners (OInt o) =
  if o `elem` map unWInt winners
    then Sum (Matches 1)
    else mempty

winCards :: Map CardId Integer -> (CardId, Matches) -> Map CardId Integer
winCards cardMap (id, Matches amount) =
  -- union is left-biased
  let amountOfThisCard = 1 + fromMaybe 0 (cardMap !? id)
      idsToUpdate = map ((+ id) . CardId) [1 .. amount]
      newMaps = map (`M.singleton` amountOfThisCard) idsToUpdate
   in M.unionsWith (+) (cardMap : (M.singleton id 1 : newMaps))

parseCard :: RE Char Card
parseCard =
  Card
    <$> ("Card" *> space *> decimal <* ":")
    <*> (space *> (map WInt <$> decimal `sepBy` space) <* space <* "|")
    <*> (space *> (map OInt <$> decimal `sepBy` space))
