module Day6Spec where

import Day6
import Test.Hspec
import Text.Regex.Applicative

spec :: Spec
spec = do
  describe "the maths" $ do
    it "counts correctly" $ do
      countInterval (1.25, 5.37) `shouldBe` 4
    it "solves correctly" $ do
      let (lo, hi) = solvePolyRoots 7 9
      (1 < lo && lo <= 2) `shouldBe` True
      (5 <= hi && hi < 6) `shouldBe` True
  describe "the parse" $ do
    it "parse times" $ do
      ("Time:        49     78     79     80" =~ parseRaceTimes) `shouldBe` Just [49, 78, 79, 80]
    it "parses dists" $ do
      ("Distance:   298   1185   1066   1181" =~ parseRaceRecords) `shouldBe` Just [298, 1185, 1066, 1181]
    it "parses races" $ do
      ("Time:        49     78     79     80\nDistance:   298   1185   1066   1181\n" =~ parseRaces) `shouldBe` Just [Race 49 298, Race 78 1185, Race 79 1066, Race 80 1181]
  describe "e2e" $ do
    it "works on the example of the handout" $ do
      puzzle1 "Time:      7  15   30\nDistance:  9  40  200\n" `shouldBe` 288
    it "foo" $ do
      countInterval (solvePolyRoots 7 9) `shouldBe` 4
      countInterval (solvePolyRoots 15 40) `shouldBe` 8
      countInterval (solvePolyRoots 30 200) `shouldBe` 9
  describe "part 2" $ do
    it "parses times" $ do
      ("Time:  7     15   30" =~ parseRaceTime2) `shouldBe` Just 71530
