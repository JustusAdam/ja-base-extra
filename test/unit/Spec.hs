module Main where


import Test.Hspec
import Data.List.JAExtra


listSpec :: Spec
listSpec = do
  describe "slice" $ do
    it "extracts a slice from index n up to, but not including index m" $
      slice 2 5 [0,1,2,3,4,5,6] `shouldBe` [2,3,4]

    it "extracts a slice from index n to index m when they are negative" $
      slice (-5) (-2) [0,1,2,3,4,5,6] `shouldBe` [2,3,4]

    it "extracts a slice when only m is negative" $
      slice 1 (-2) [1,2,3,4,5,6] `shouldBe` [2,3,4]

    it "extracts a slice when only n is negative" $
      slice (-5) 4 [1,2,3,4,5,6] `shouldBe` [2,3,4]

    it "returns the rest of the list if m is positively out of bounds" $
      slice 1 10 [1,2,3,4] `shouldBe` [2,3,4]

    it "returns the empty list if m is negatively out of bounds" $
      slice 1 (-10) [1,2,3,4] `shouldBe` []

    it "returns the empty list if n is positively out of bounds" $
      slice 100 3 [1,2,3,4] `shouldBe` []

    it "returns the entire head of the list if n is negatively out of bounds" $
      slice (-100) 2 [1,2,3,4] `shouldBe` [1,2]

  describe "setIndex" $ do
    it "sets the index if it exists" $
      setIndex 1 'e' "hallo" `shouldBe` Just "hello"

    it "sets the index if it is negative" $
      setIndex (-4) 'e' "hallo" `shouldBe` Just "hello"

    it "fails if the index is positively out of bounds" $
      setIndex 10 'e' "hallo" `shouldBe` Nothing

    it "fails if the index is negatively out of bounds" $
      setIndex (-10) 'e' "hallo" `shouldBe` Nothing

  describe "setPred" $ do
    it "sets the element where the predicate is true" $
      setPred (== 'a') 'e' "hallo" `shouldBe` Just "hello"

    it "sets the element where the predicate is first true" $
      setPred (== 'a') 'e' "hallo amanda" `shouldBe` Just "hello amanda"

    it "fails if nothing matches the predicate" $
      setPred (== 'a') 'e' "hello" `shouldBe` Nothing


main :: IO ()
main =
  hspec listSpec
