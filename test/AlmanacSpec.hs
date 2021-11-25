module AlmanacSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "test" $ do
    it "works" $ do
      42 `shouldBe` 42
