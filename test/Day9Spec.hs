module Day9Spec where

import Test.Hspec
import Day9
import Text.Regex.Applicative

spec :: Spec
spec = do
  describe "the parsing" $ do
    it "parses a line with negatives" $ do
      match parseSequence "16 33 50 64 72 76 90 143 264 419 340 -868 -5538 -18941 -51661 -122356 -258160 -484127 -779910 -954772 -340276"
      `shouldBe`
      Just [16, 33, 50, 64, 72, 76, 90, 143, 264, 419, 340, -868, -5538, -18941, -51661, -122356, -258160, -484127, -779910, -954772, -340276]
  describe "the running" $ do
    it "works with all three sequences in the handout" $ do
      findNextInSequence [0, 3, 6, 9, 12, 15] `shouldBe` 18
      findNextInSequence [1, 3, 6, 10, 15, 21] `shouldBe` 28
      findNextInSequence [10, 13, 16, 21, 30, 45] `shouldBe` 68
    it "puzzle 2 on all three handout sequences" $ do
      findPrevInSequence [0, 3, 6, 9, 12, 15] `shouldBe` -3
      findPrevInSequence [1, 3, 6, 10, 15, 21] `shouldBe` 0
      findPrevInSequence [10, 13, 16, 21, 30, 45] `shouldBe` 5
