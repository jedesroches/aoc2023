module Day2Spec where

import Day2
import Test.Hspec
import Text.Regex.Applicative

spec :: Spec
spec = do
  describe "Problem 1" $ do
    it "handout example" $ do
      True `shouldBe` True
  describe "parseRed" $ do
    it "parses red cubes" $ do
      ("30 red" =~ parseRed) `shouldBe` Just (Red 30)
    it "does not parse blue cubes" $ do
      ("30 blue" =~ parseRed) `shouldBe` Nothing
  describe "parseBlue" $ do
    it "parses blue cubes" $ do
      ("30 blue" =~ parseBlue) `shouldBe` Just (Blue 30)
    it "does not parse red cubes" $ do
      ("30 red" =~ parseBlue) `shouldBe` Nothing
  describe "parseDraw" $ do
    it "parses a single color" $ do
      ("3 red" =~ parseDraw) `shouldBe` Just [R (Red 3)]
    it "parses two colors with a ," $ do
      ("3 red, 4 blue" =~ parseDraw) `shouldBe` Just [R (Red 3), B (Blue 4)]
  describe "parseDraws" $ do
    it "parses a single draw" $ do
      ("3 red, 4 blue" =~ parseDraws) `shouldBe` Just [[R (Red 3), B (Blue 4)]]
    it "parses two draws" $
      do
        "3 red, 4 blue; 5 green" =~ parseDraws
        `shouldBe` Just [[R (Red 3), B (Blue 4)], [G (Green 5)]]
  describe "maxColors" $ do
    it "fills the nothings" $
      do
        maxColors [[R (Red 3), B (Blue 4), G (Green 5)]]
        `shouldBe` (Just (Red 3), Just (Green 5), Just (Blue 4))
    it "maxes out of several draws" $
      do
        maxColors [[R (Red 1), B (Blue 1), G (Green 1)], [R (Red 2), B (Blue 2), G (Green 2)]]
        `shouldBe` (Just (Red 2), Just (Green 2), Just (Blue 2))
  describe "end to end" $ do
    let input =
          [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
          ]
    it "parses the handout example" $
      do
        match parseGame (head input)
        `shouldBe` Just (Game (GameId 1) (Just (Red 4), Just (Green 2), Just (Blue 6)))
    it "works with the handout" $ do
      puzzle1 input `shouldBe` 8
    it "builds the right game of complicated input" $
      do
        match parseGame "Game 84: 3 blue, 4 green, 9 red; 7 blue, 13 green, 17 red; 14 blue, 16 green, 14 red; 7 blue, 16 green, 3 red; 9 red, 2 blue, 14 green; 13 blue, 3 green, 7 red"
        `shouldBe` Just (Game (GameId 84) (Just (Red 17), Just (Green 16), Just (Blue 14)))
    it "gives a valid game" $
      do
        gameValueIfPossible (Game (GameId 14) (Just (Red 12), Just (Green 13), Just (Blue 14)))
        `shouldBe` Just 14
    it "gives a Nothing game" $
      do
        gameValueIfPossible (Game (GameId 1) (Nothing, Nothing, Nothing))
        `shouldBe` Just 1
    it "works on some complicated random cases" $
      do
        puzzle1
          [ "Game 12: 16 green, 1 blue; 1 green, 4 blue; 1 red, 2 blue, 16 green; 17 green, 3 blue; 14 green, 4 blue, 3 red; 7 green, 8 blue"
          , "Game 84: 3 blue, 4 green, 9 red; 7 blue, 13 green, 17 red; 14 blue, 16 green, 14 red; 7 blue, 16 green, 3 red; 9 red, 2 blue, 14 green; 13 blue, 3 green, 7 red"
          , "Game 3: 1 blue, 1 green, 1 red"
          ]
        `shouldBe` 3
