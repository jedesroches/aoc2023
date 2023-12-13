{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}

module Day6 where

import Adventlib
import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Data.Maybe (fromJust)

puzzle1 :: String -> Integer
puzzle1 = product . map solveRace . fromJust . match parseRaces

puzzle2 :: String -> Integer
puzzle2 = solveRace . fromJust . match parseRace2

newtype RaceTime = RaceTime Integer
  deriving (Eq, Show, Ord, Enum, Num, Real, Integral)

newtype RaceDistance = RaceDistance Integer
  deriving (Eq, Show, Ord, Enum, Num, Real, Integral)

data Race = Race
  { _time :: RaceTime,
    _record :: RaceDistance
  }
  deriving (Eq, Show)

solveRace :: Race -> Integer
solveRace (Race t r) = countInterval $ solvePolyRoots t r

solvePolyRoots :: RaceTime -> RaceDistance -> (Double, Double)
solvePolyRoots y z =
  let y' = fromIntegral y
      z' = fromIntegral z
      sqrtDelta = sqrt (y' ^ 2 - 4 * z')
   in (0.5 * (y' - sqrtDelta), 0.5 * (y' + sqrtDelta))

countInterval :: (RealFrac a) => (a, a) -> Integer
countInterval (lo, hi) = ceiling (hi - 1) - floor (lo + 1) + 1

parseRaces :: RE Char [Race]
parseRaces = zipWith Race <$> (parseRaceTimes <* "\n") <*> parseRaceRecords <* "\n"

parseRace2 :: RE Char Race
parseRace2 = Race <$> (parseRaceTime2 <* "\n") <*> (parseRaceRecord2 <* "\n")

parseRaceTimes :: RE Char [RaceTime]
parseRaceTimes = "Time:" *> many space *> decimal `sepBy` many " "

parseRaceTime2 :: RE Char RaceTime
parseRaceTime2 = base10 <$> ("Time:" *> many space *> digit `sepBy` (many " " <|> mempty))

parseRaceRecords :: RE Char [RaceDistance]
parseRaceRecords = "Distance:" *> many space *> decimal `sepBy` many " "

parseRaceRecord2 :: RE Char RaceDistance
parseRaceRecord2 = base10 <$> ("Distance:" *> many space *> digit `sepBy` (many " " <|> mempty))
