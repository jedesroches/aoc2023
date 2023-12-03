module Day2Spec where

import Test.Hspec
-- import Day2

puzzle1 = undefined

spec :: Spec
spec = do
  describe "Problem 1" $ do
    it "handout example" $ do
      let input = [""]
      puzzle1 input `shouldBe` Just 0
