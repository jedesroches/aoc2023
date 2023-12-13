module Day4Spec where

import Day4
import Test.Hspec
import Text.Regex.Applicative

spec :: Spec
spec = do
  describe "points for card" $ do
    it "returns 0 points for 0 matches" $ do
      pointsForMatches (Matches 0) `shouldBe` 0
    it "returns 0 points for negative matches" $ do
      pointsForMatches (Matches $ -3) `shouldBe` 0
    it "returns 8 points for four matches" $ do
      pointsForMatches (Matches 4) `shouldBe` 8
  describe "matches for card" $ do
    it "returns 0 for empty winner list" $ do
      matchesForCard (Card 0 [] [OInt 3, OInt 5]) `shouldBe` (CardId 0, 0)
    it "returns 1 for one match" $ do
      matchesForCard (Card 0 [WInt 3] [OInt 3, OInt 4]) `shouldBe` (CardId 0, 1)
    it "returns 2 for two matches" $ do
      matchesForCard (Card 0 [WInt 5, WInt 27] [OInt 10, OInt 5, OInt 27]) `shouldBe` (CardId 0, 2)
  describe "parseCard" $ do
    it "parses an example card" $
      do
        "Card 1: 41 48 83 86 17 | 83 86 6 31 17 9 48 53" =~ parseCard
        `shouldBe` Just (Card 1 (map WInt [41, 48, 83, 86, 17]) (map OInt [83, 86, 6, 31, 17, 9, 48, 53]))
    it "works with multiple spaces" $ do
      "Card 1: 1  3  5 | 3" =~ parseCard
      `shouldBe`
      Just (Card 1 (map WInt [1, 3, 5]) [OInt 3])
  describe "puzzle2 e2e" $ do
    it "works on handout" $ do
      puzzle2 . lines $ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
      `shouldBe`
      30
