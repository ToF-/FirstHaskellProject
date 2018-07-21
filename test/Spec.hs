import Test.Hspec
import WordLadder

main :: IO ()
main = hspec $ do
    describe "adjacent words" $ do
        it "cannot be identical" $ do
            ("DOG" `adjacent` "DOG") `shouldBe` False

        it "cannot be of different size" $ do
          ("DOG" `adjacent` "DOGE") `shouldBe` False

        it "differ by one letter only" $ do
          ("DOG" `adjacent` "COG") `shouldBe` True
          ("DOG" `adjacent` "CAT") `shouldBe` False

