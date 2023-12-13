{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-identities #-}

module Day5 where

import Adventlib
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Protolude hiding (Location)
import Protolude.Partial (fromJust)
import Text.Regex.Applicative
import Text.Regex.Applicative.Common

puzzle1 :: String -> Location
puzzle1 = minimum . locationsOfSeeds . fromJust . match (parseAlmanac parseSeeds1)

puzzle2 :: String -> Location
puzzle2 = fromJust . smallestExistingLocation . fromJust . match (parseAlmanacR parseSeeds2)

locationsOfSeeds :: Almanac -> Set Location
locationsOfSeeds alm = S.map (seedToLocation alm) (seedList alm)

smallestExistingLocation :: AlmanacR -> Maybe Location
smallestExistingLocation alm =
  find (isInRanges (seedRanges alm) . locationToSeed alm) (map Location [1..])

newtype Seed = Seed Integer
  deriving (Show, Eq, Ord, Real, Enum, Integral, Num)

newtype Soil = Soil Integer
  deriving (Show, Eq, Ord, Real, Enum, Integral, Num)

newtype Fertilizer = Fertilizer Integer
  deriving (Show, Eq, Ord, Real, Enum, Integral, Num)

newtype Water = Water Integer
  deriving (Show, Eq, Ord, Real, Enum, Integral, Num)

newtype Light = Light Integer
  deriving (Show, Eq, Ord, Real, Enum, Integral, Num)

newtype Temperature = Temperature Integer
  deriving (Show, Eq, Ord, Real, Enum, Integral, Num)

newtype Humidity = Humidity Integer
  deriving (Show, Eq, Ord, Real, Enum, Integral, Num)

newtype Location = Location Integer
  deriving (Show, Eq, Ord, Real, Enum, Integral, Num)

data Almanac = Almanac
  { seedList :: Set Seed,
    _seedToSoil :: Set (Range Seed Soil),
    _soilToFertilizer :: Set (Range Soil Fertilizer),
    _fertilizerToWater :: Set (Range Fertilizer Water),
    _waterToLight :: Set (Range Water Light),
    _lightToTemp :: Set (Range Light Temperature),
    _tempToHumidity :: Set (Range Temperature Humidity),
    _humidityToLocation :: Set (Range Humidity Location)
  }

data AlmanacR = AlmanacR
  { seedRanges :: Set (Range Seed Seed)
  , _a :: Set (Range Soil Seed)
  , _b :: Set (Range Fertilizer Soil)
  , _c :: Set (Range Water Fertilizer)
  , _d :: Set (Range Light Water)
  , _e :: Set (Range Temperature Light)
  , _f :: Set (Range Humidity Temperature)
  , _g :: Set (Range Location Humidity)
  }

data Range a b = Range
  { low :: !a,
    isInRange :: a -> Bool,
    getMapped :: a -> b
  }

instance (Eq a) => Eq (Range a b) where
  (==) a b = low a == low b

instance (Ord a) => Ord (Range a b) where
  compare a b = compare (low a) (low b)

buildRange :: (Integral a, Num b) => b -> a -> Integer -> Range a b
buildRange destStart srcStart rangeLen =
  let mapFunc a = destStart + fromIntegral (a - srcStart)
      inRange a = srcStart <= a && a < srcStart + fromInteger rangeLen
   in Range srcStart inRange mapFunc

findMapped :: (Integral a, Num b) => Set (Range a b) -> a -> b
findMapped ranges a =
  case S.lookupLE (Range a undefined undefined) ranges of
    Just candidateRange | isInRange candidateRange a -> getMapped candidateRange a
    _ -> fromIntegral a

isInRanges :: (Ord a) => Set (Range a a) -> a -> Bool
isInRanges ranges a =
  case S.lookupLE (Range a undefined undefined) ranges of
    Just candidateRange | isInRange candidateRange a -> True
    _ -> False

seedToLocation :: Almanac -> Seed -> Location
seedToLocation
  ( Almanac
      _
      seedToSoil
      soilToFertilizer
      fertilizerToWater
      waterToLight
      lightToTemp
      tempToHumidity
      humidityToLocation
    ) =
    findMapped humidityToLocation
      . findMapped tempToHumidity
      . findMapped lightToTemp
      . findMapped waterToLight
      . findMapped fertilizerToWater
      . findMapped soilToFertilizer
      . findMapped seedToSoil

locationToSeed :: AlmanacR -> Location -> Seed
locationToSeed
  ( AlmanacR
    _
    soilToSeed
    fertilizerToSoil
    waterToFertilizer
    lightToWater
    tempToLight
    humidToTemp
    locToHumid
  ) = findMapped soilToSeed
    . findMapped fertilizerToSoil
    . findMapped waterToFertilizer
    . findMapped lightToWater
    . findMapped tempToLight
    . findMapped humidToTemp
    . findMapped locToHumid

parseSeeds1 :: RE Char (Set Seed)
parseSeeds1 = fmap S.fromList $ "seeds: " *> decimal `sepBy` " "

parseSeeds2 :: RE Char (Set (Range Seed Seed))
parseSeeds2 = fmap S.fromList $ "seeds: " *> (buildList <$> decimal <*> (" " *> decimal)) `sepBy` " "
  where
    buildList start = buildRange start start

parseRange ::
  (Num i, Integral a, Num b) =>
  (i -> b) -> -- Constructor for dest type
  (i -> a) -> -- Constructor for src type
  RE Char (Range a b)
parseRange dstTyp srcTyp =
  buildRange
    <$> ((dstTyp <$> decimal) <* space)
    <*> ((srcTyp <$> decimal) <* space)
    <*> decimal

parseRangeR ::
  (Num i, Integral a, Num b) =>
  (i -> b) -> 
  (i -> a) -> 
  RE Char (Range a b)
parseRangeR dstTyp srcTyp =
  flip buildRange
    <$> ((srcTyp <$> decimal) <* space)
    <*> ((dstTyp <$> decimal) <* space)
    <*> decimal

parseMap ::
  (Num i, Integral a, Num b) =>
  String -> -- Section header
  (i -> b) -> -- Constructor for dest type
  (i -> a) -> -- Constructor for src type
  RE Char (Set (Range a b))
parseMap s dstType srcType =
  string (s ++ " map:\n")
    *> (S.fromList <$> parseRange dstType srcType `sepBy` "\n")

parseMapR ::
  (Num i, Integral a, Num b) =>
  String -> -- Section header
  (i -> b) -> -- Constructor for dest type
  (i -> a) -> -- Constructor for src type
  RE Char (Set (Range a b))
parseMapR s dstType srcType =
  string (s ++ " map:\n")
    *> (S.fromList <$> parseRangeR dstType srcType `sepBy` "\n")

parseAlmanac :: RE Char (Set Seed) -> RE Char Almanac
parseAlmanac parseSeeds =
  Almanac
    <$> parseSeeds
    <*> ("\n\n" *> parseMap "seed-to-soil" Soil Seed)
    <*> ("\n\n" *> parseMap "soil-to-fertilizer" Fertilizer Soil)
    <*> ("\n\n" *> parseMap "fertilizer-to-water" Water Fertilizer)
    <*> ("\n\n" *> parseMap "water-to-light" Light Water)
    <*> ("\n\n" *> parseMap "light-to-temperature" Temperature Light)
    <*> ("\n\n" *> parseMap "temperature-to-humidity" Humidity Temperature)
    <*> ("\n\n" *> parseMap "humidity-to-location" Location Humidity <* some "\n")

parseAlmanacR :: RE Char (Set (Range Seed Seed)) -> RE Char AlmanacR
parseAlmanacR parseSeeds =
  AlmanacR
    <$> parseSeeds
    <*> ("\n\n" *> parseMapR "seed-to-soil" Seed Soil)
    <*> ("\n\n" *> parseMapR "soil-to-fertilizer" Soil Fertilizer)
    <*> ("\n\n" *> parseMapR "fertilizer-to-water" Fertilizer Water)
    <*> ("\n\n" *> parseMapR "water-to-light" Water Light)
    <*> ("\n\n" *> parseMapR "light-to-temperature" Light Temperature)
    <*> ("\n\n" *> parseMapR "temperature-to-humidity" Temperature Humidity)
    <*> ("\n\n" *> parseMapR "humidity-to-location" Humidity Location <* some "\n")
