module Day1Spec where

import Test.Hspec
import Day1

spec :: Spec
spec = do
  describe "Puzzle 1" $ do
    it "Handout example" $ do
      puzzle1 ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"] `shouldBe` Just 142
    it "End to end works" $ do
      inputs <- readFile "inputs/input1.txt"
      puzzle1 (lines inputs) `shouldBe` Just 55002
  describe "Puzzle 2" $ do
    let inputs = [ "two1nine"
                 , "eightwothree"
                 , "abcone2threexyz"
                 , "xtwone3four"
                 , "4nineeightseven2"
                 , "zoneight234"
                 , "7pqrstsixteen"
                 ]
    it "Handout example" $ do
      puzzle2 inputs `shouldBe` Just 281
    it "findFirst works" $ do
      map findFirst inputs `shouldBe` map Just [ '2' , '8' , '1' , '2' , '4' , '1' , '7' ]
    it "findLast works" $ do
      map findLast inputs `shouldBe` map Just ['9', '3', '3', '4', '2', '4', '6']
    it "end to end works" $ do
      fullInput <- readFile "inputs/input1.txt"
      puzzle2 (lines fullInput) `shouldBe` Just 55093
