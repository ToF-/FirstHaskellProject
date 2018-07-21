import Test.Hspec
import WordLadder
import Data.List (sort)

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

    describe "word graph" $ do
        it "is a graph of adjacent words" $ do
            let ws = words "DOG COG CAT BAT BUG BAG FOO COT"
            let sorted = sort . map (fmap (unwords . sort)) . toList
            let gs = sorted $ wordGraph ws
            gs `shouldBe` [("BAG","BAT BUG")
                          ,("BAT","BAG CAT")
                          ,("BUG","BAG")
                          ,("CAT","BAT COT")
                          ,("COG","COT DOG")
                          ,("COT","CAT COG")
                          ,("DOG","COG")
                          ,("FOO", "")]

