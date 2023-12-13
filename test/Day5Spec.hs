{-# LANGUAGE ScopedTypeVariables #-}

module Day5Spec where

import Day5

import qualified Data.Set as S
import Data.Maybe
import Test.Hspec
import Text.Regex.Applicative

spec :: Spec
spec = do
  describe "buildRange" $ do
    it "Range seems ordered" $ do
      Range (Seed 5) undefined undefined < Range (Seed 7) undefined undefined
      `shouldBe`
      True
    it "builds a simple range" $ do
      let myRange = buildRange (Soil 7) (Seed 5)  10
      low myRange `shouldBe` 5
      all (isInRange myRange) [5..14] `shouldBe` True
      any (isInRange myRange) [3,4,15,16] `shouldBe` False
      map (getMapped myRange) [6,7,8] `shouldBe` [8,9,10]
  describe "findMapped" $ do
    it "works with the handout" $ do
      let range1 = buildRange (Soil 50) (Seed 98) 2
          range2 = buildRange (Soil 52) (Seed 50) 48
          ranges = S.fromList [range1, range2]
      map (findMapped ranges . Seed) [97, 98, 99, 100] `shouldBe` [99, 50, 51, 100]
  describe "parseMap" $ do
    it "parses a simple example" $ do
      low <$> ("50 98 2" =~ parseRange Soil Seed) `shouldBe` Just 98
    it "parses a range" $ do
      S.map low <$> ("seed-to-soil map:\n50 98 2\n52 50 48" =~ parseMap "seed-to-soil" Seed Soil) `shouldBe` Just (S.fromList [98, 50])
  describe "parseAlmanac" $ do
    it "parses the handout exmaple" $ do
      fullInput <- readFile "inputs/input5-small.txt"
      (\(a :: String, b :: Almanac, c :: String) -> (a, c)) <$> findLongestInfix (parseAlmanac parseSeeds1) fullInput `shouldBe` Just ("", "")
