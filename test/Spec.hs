import Test.Hspec
import WordLadder
import Data.List (sort)

main :: IO ()
main = hspec $ do
    let ws = sort $ words "BOG CAT DOG COG BAT BUG BAG QUX COT FOG FAT"
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
            let sorted = sort . map (fmap (unwords . sort)) . toList
            let gs = sorted $ wordGraph ws
            gs `shouldBe` [("BAG","BAT BOG BUG")
                          ,("BAT","BAG CAT FAT")
                          ,("BOG","BAG BUG COG DOG FOG")
                          ,("BUG","BAG BOG")
                          ,("CAT","BAT COT FAT")
                          ,("COG","BOG COT DOG FOG")
                          ,("COT","CAT COG")
                          ,("DOG","BOG COG FOG")
                          ,("FAT","BAT CAT")
                          ,("FOG","BOG COG DOG")
                          ,("QUX","")]

    describe "word ladder" $ do
        let ladder_ ws s t = unwords $ ladder ws s t
        describe "returns a path from a word to another word" $ do
            it "for two adjacents words" $ do
                ladder_ ws "BAG" "BAT" `shouldBe` "BAG BAT"
                ladder_ ws "DOG" "COG" `shouldBe` "DOG COG"

            it "for three adjacents words" $ do
                ladder_ ws "BAG" "CAT" `shouldBe` "BAG BAT CAT"

            it "that is the shortest path" $ do
                ladder_ ws "DOG" "CAT" `shouldBe` "DOG COG COT CAT"

        describe "returns an empty path" $ do
            it "when the words are of different size" $ do
                ladder ws "BAG" "BATH" `shouldBe` []

            it "when the words are identical" $ do
                ladder ws "BAG" "BAG" `shouldBe` []

            it "when the words are *not* in the word list" $ do
                ladder ws "BIG" "BAG" `shouldBe` []
                ladder ws "BAG" "BIG" `shouldBe` []

            it "when a path cannot be found" $ do
                ladder ws "FOO" "BAG" `shouldBe` []
                ladder ws "BAG" "FOO" `shouldBe` []




