import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "some feature" $ do
        it "should work" $ do
            2 + 2 `shouldBe` 4
    describe "some other feature" $ do
      it "should also work" $ do
       2+2 `shouldBe` 5