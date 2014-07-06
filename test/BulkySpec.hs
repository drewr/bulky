module BulkySpec (main, spec) where

import Test.Hspec
import Bulky (foo)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      foo `shouldBe` bar
  where bar = 3

