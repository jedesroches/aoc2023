{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Used otherwise as a pattern" #-}

module Day7 where

import Adventlib
import Data.Functor
import qualified Data.List.NonEmpty as NE 
import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Data.Maybe
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid
import Data.List (sort)

puzzle1 :: [String] -> Integer
puzzle1 = getSum . foldMap idxTimesBid . zip [1..] . sort . parseIt
  where parseIt = map (fromJust . match parseHand)

puzzle2 :: [String] -> Integer
puzzle2 = getSum . foldMap idxTimesBid . zip [1..] . sort . parseIt
  where parseIt = map (fromJust . match parseHand2)

idxTimesBid :: (Integer, Hand a) -> Sum Integer
idxTimesBid (i, h) = Sum (i * bid h)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A
  deriving (Eq, Show, Ord)

  
data Card2 = J2 | Two2 | Three2 | Four2 | Five2 | Six2 | Seven2 | Eight2 | Nine2 | T2 | Q2 | K2 | A2
  deriving (Eq, Show, Ord)

type HandOfCards a = (a, a, a, a, a)

data HandType
  = HighCard
  | OnePair
  | TwoPairs
  | ThreeSame
  | FullHouse
  | FourSame
  | FiveSame
  deriving (Eq, Show, Ord)

data Hand a = Hand
  { _cards :: HandOfCards a,
    _type :: HandType,
    bid :: Integer
  }
  deriving (Show)

instance Eq a => Eq (Hand a) where
  (==) phl phr = _cards phl == _cards phr

instance Ord a => Ord (Hand a) where
  compare phl phr = case compare (_type phl) (_type phr) of
    EQ -> compare (_cards phl) (_cards phr)
    other -> other

-- Some fun with types at the expense, I admit, of readability of the patterns.
handType :: HandOfCards Card -> HandType
handType (c1, c2, c3, c4, c5) =
  let groupedHand = NE.group1 . NE.sort . NE.fromList $ [c1, c2, c3, c4, c5] -- NonEmpty (NonEmpty Card)
   in case groupedHand of
        _ :| [] -> FiveSame
        x :| [_]
          | length x == 1 || length x == 4 -> FourSame
          | otherwise -> FullHouse
        x :| [y, z]
          | lengthsOr3 x y z 1 1 3 -> ThreeSame
          | otherwise -> TwoPairs
        _ :| [_, _, _] -> OnePair
        _ -> HighCard

handType2 :: HandOfCards Card2 -> HandType
handType2 (c1, c2, c3, c4, c5) =
  let groupedHand = NE.group . sort . filter (/= J2) $ [c1, c2, c3, c4, c5]
  in case groupedHand of
    [] -> FiveSame
    [_] -> FiveSame -- only one equality group after filtering the J
    [x, y]
      | lengthsOr x y 4 1 -> FourSame
      | lengthsOr x y 3 1 -> FourSame
      | lengthsOr x y 3 2 -> FullHouse
      | lengthsOr x y 2 2 -> FullHouse
      | lengthsOr x y 2 1 -> FourSame
      | otherwise -> FourSame
    [x, y, z]
      | lengthsOr3 x y z 3 1 1 -> ThreeSame
      | lengthsOr3 x y z 2 2 1 -> TwoPairs
      | lengthsOr3 x y z 2 1 1 -> ThreeSame
      | otherwise -> ThreeSame
    [_, _, _, _] -> OnePair
    _ -> HighCard

lengthsOr :: NonEmpty a -> NonEmpty b -> Int -> Int -> Bool
lengthsOr x y r1 r2 = let x' = length x
                          y' = length y
                      in (x' == r1 && y' == r2) || (y' == r1 && x' == r2)

lengthsOr3 :: NonEmpty a -> NonEmpty b -> NonEmpty c -> Int -> Int -> Int -> Bool
lengthsOr3 x y z r1 r2 r3 = let x' = length x
                                y' = length y
                                z' = length z
                            in (x' == r1 && y' == r2 && z' == r3) || (x' == r1 && y' == r3 && z' == r2)
                                || (x' == r2 && y' == r1 && z' == r3) || (x' == r2 && y' == r3 && z' == r2)
                                || (x' == r3 && y' == r1 && z' == r2) || (x' == r3 && y' == r2 && z' == r1)

parseHand :: RE Char (Hand Card)
parseHand =
    (\c b -> Hand c (handType c) b)
      <$> parseCards parseCard
      <*> (space *> decimal)

parseHand2 :: RE Char (Hand Card2)
parseHand2 =
  (\c b -> Hand c (handType2 c) b)
    <$> parseCards parseCard2
    <*> (space *> decimal)

parseCards :: RE Char a -> RE Char (a, a, a, a, a)
parseCards parser = (,,,,) <$> parser <*> parser <*> parser <*> parser <*> parser

parseCard2 :: RE Char Card2
parseCard2 =
  "2" $> Two2
    <|> "3" $> Three2
    <|> "4" $> Four2
    <|> "5" $> Five2
    <|> "6" $> Six2
    <|> "7" $> Seven2
    <|> "8" $> Eight2
    <|> "9" $> Nine2
    <|> "T" $> T2
    <|> "J" $> J2
    <|> "Q" $> Q2
    <|> "K" $> K2
    <|> "A" $> A2

parseCard :: RE Char Card
parseCard =
  "2" $> Two
    <|> "3" $> Three
    <|> "4" $> Four
    <|> "5" $> Five
    <|> "6" $> Six
    <|> "7" $> Seven
    <|> "8" $> Eight
    <|> "9" $> Nine
    <|> "T" $> T
    <|> "J" $> J
    <|> "Q" $> Q
    <|> "K" $> K
    <|> "A" $> A 
