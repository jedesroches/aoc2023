{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day2 where

import Data.String
import Adventlib
import Protolude
import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Protolude.Partial (fromJust)

puzzle1 :: [String] -> Integer
puzzle1 = sumOfPossibleGames . map (fromJust . match parseGame)

puzzle2 :: [String] -> Integer
puzzle2 = sum . map (powerOfGame . fromJust . match parseGame)

newtype Red = Red {getRed :: Integer}
  deriving (Eq, Show, Num, Ord)

newtype Blue = Blue {getBlue :: Integer}
  deriving (Eq, Show, Num, Ord)

newtype Green = Green {getGreen :: Integer}
  deriving (Eq, Show, Num, Ord)

data Color
  = R Red
  | B Blue
  | G Green
  deriving (Eq, Show)

data Draw = Draw
  { redCubes :: Maybe Red,
    blueCubes :: Maybe Blue,
    greenCubes :: Maybe Green
  }
  deriving (Eq, Show)

newtype GameId = GameId Integer
  deriving (Num, Show, Eq)

type MaxColorInformation = (Maybe Red, Maybe Green, Maybe Blue)

data Game = Game
  { _gameId   :: !GameId
  , _maxCubes :: !MaxColorInformation
  } deriving (Show, Eq)


sumOfPossibleGames :: [Game] -> Integer
sumOfPossibleGames = sum . mapMaybe gameValueIfPossible

powerOfGame :: Game -> Integer
powerOfGame (Game _ (red, green, blue)) = getRed (fromMaybe 1 red) * getGreen (fromMaybe 1 green) * getBlue (fromMaybe 1 blue)

gameValueIfPossible :: Game -> Maybe Integer
gameValueIfPossible (Game (GameId gid) (red, green, blue)) =
  if (fromMaybe 0 red <= 12) && (fromMaybe 0 green <= 13) && (fromMaybe 0 blue <= 14)
  then Just gid
  else Nothing

maxColors :: [[Color]] -> MaxColorInformation
maxColors = foldl' maxForDraw (Nothing, Nothing, Nothing)
  where maxForDraw :: MaxColorInformation -> [Color] -> MaxColorInformation
        maxForDraw = foldl' writeIfMax

        writeIfMax (Just r, g, b) (R red) | red > r = (Just red, g, b)
        writeIfMax (Nothing, g, b) (R red) = (Just red, g, b)

        writeIfMax (r, g, Just b) (B blue)  | blue > b = (r, g, Just blue)
        writeIfMax (r, g, Nothing) (B blue) = (r, g, Just blue)

        writeIfMax (r, Just g, b) (G green)  | green > g = (r, Just green, b)
        writeIfMax (r, Nothing, b) (G green) = (r, Just green, b)

        writeIfMax currentMax _ = currentMax

parseGame :: RE Char Game
parseGame = Game <$> ("Game " *> (GameId <$> decimal <* ": ")) <*> (maxColors <$> parseDraws)

parseDraws :: RE Char [[Color]]
parseDraws = parseDraw `sepBy` "; "

parseDraw :: RE Char [Color]
parseDraw = parseColor `sepBy` ", "

parseColor :: RE Char Color
parseColor =
  R <$> parseRed
    <|> G <$> parseGreen
    <|> B <$> parseBlue

parseRed :: RE Char Red
parseRed = Red <$> decimal <* " red"

parseBlue :: RE Char Blue
parseBlue = Blue <$> decimal <* " blue"

parseGreen :: RE Char Green
parseGreen = Green <$> decimal <* " green"
