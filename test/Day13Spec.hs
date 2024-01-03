module Day13Spec where

import Test.Hspec
import Day13

spec :: Spec
spec = do
  describe "The reflector detection" $ do
    let line1A = ".##.#"
        line1B = ".##."
        line2A = "###"
        line2B = "..."
        line3A = "#.#.#"
        line3B = "#.#."
    it "validates reflections correctly" $ do
      isReflection line1A line1B `shouldBe` True
      isReflection line2A line2B `shouldBe` False
      isReflection line3A line3B `shouldBe` True
