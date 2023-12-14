module AdventlibSpec where
import Test.Hspec
import Adventlib

spec :: Spec
spec = do
  describe "countWhile" $ do
    it "counts to 10" $ do
      countWhile (/= 10) [0..] `shouldBe` 10
