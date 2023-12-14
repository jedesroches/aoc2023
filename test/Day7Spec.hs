module Day7Spec where

import Data.List
import Data.Tuple.Extra
import Day7
import Test.Hspec

spec :: Spec
spec = do
  describe "Comparing" $ do
    it "out of hand, a full house > three of a kind" $ do
      FullHouse > ThreeSame `shouldBe` True
    it "a full house > three of a kind" $ do
      compare (Hand undefined FullHouse undefined :: Hand Card) (Hand undefined ThreeSame undefined) `shouldBe` GT
    it "second order rule works" $
      do
        compare
          (Hand (Three, Three, Three, Three, Two) FourSame 0)
          (Hand (Two, A, A, A, A) FourSame 0)
        `shouldBe` GT
    it "again" $
      do
        compare
          (Hand (Seven, Seven, Seven, Eight, Eight) ThreeSame 0)
          (Hand (Seven, Seven, Eight, Eight, Eight) ThreeSame 0)
        `shouldBe` LT
  describe "Card type parsing" $ do
    let h1 = (Three, Two, T, Three, K)
        h2 = (T, Five, Five, J, Five)
        h3 = (K, K, Six, Seven, Seven)
        h4 = (K, T, J, J, T)
        h5 = (Q, Q, Q, J, A)
    it "works on the handout" $ do
      handType h1 `shouldBe` OnePair
      handType h3 `shouldBe` TwoPairs
      handType (Q, Q, Q, J, J) `shouldBe` FullHouse
      handType (Q, Q, Q, J, A) `shouldBe` ThreeSame
    it "sorts as expected" $
      do
        sort (map (uncurry3 Hand) [(h1, handType h1, 0), (h2, handType h2, 0), (h3, handType h3, 0), (h4, handType h4, 0), (h5, handType h5, 0)])
        `shouldMatchList` [ Hand h1 OnePair 0,
                            Hand h4 TwoPairs 0,
                            Hand h3 TwoPairs 0,
                            Hand h2 ThreeSame 0,
                            Hand h5 ThreeSame 0
                          ]
  describe "e2e" $ do
    it "works on example" $
      do
        puzzle1 ["32T3K 765", "T55J5 684", "KK677 28", "KTJJT 220", "QQQJA 483"]
        `shouldBe` 6440
  describe "part 2" $ do
    let h1 = (Three2, Two2, T2, Three2, K2)
        h2 = (T2, Five2, Five2, J2, Five2)
        h3 = (K2, K2, Six2, Seven2, Seven2)
        h4 = (K2, T2, J2, J2, T2)
        h5 = (Q2, Q2, Q2, J2, A2)
    it "uses jokers correctly" $ do
        handType2 h1 `shouldBe` OnePair
        handType2 h2 `shouldBe` FourSame
        handType2 h3 `shouldBe` TwoPairs
        handType2 h4 `shouldBe` FourSame
        handType2 h5 `shouldBe` FourSame
    it "handout" $ do
      puzzle2  ["32T3K 765", "T55J5 684", "KK677 28", "KTJJT 220", "QQQJA 483"]
      `shouldBe`
      5905
