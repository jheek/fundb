
import Map
import Test.Hspec
import Test.QuickCheck


ordered :: Ord k => [(k, v)] -> Bool
ordered xs = and (zipWith (\x y -> fst x < fst y) xs (drop 1 xs))

prop_OrderedEntries :: [(Int, Int)] -> Bool
prop_OrderedEntries xs = ordered . entries . makeMap $ xs

main :: IO ()
main = hspec $ do
  describe "map" $ do
    it "entries should be ordered" $ property prop_OrderedEntries
    describe "split" $ do
      let s = 1000
      let set = makeSet [(1 :: Int)..s]
      let piv = 123
      it "should split left items" $
        mapKeys (splitL piv set) `shouldBe` [1..(piv - 1)]
      it "should split right items" $
        mapKeys (splitR piv set) `shouldBe` [(piv + 1)..s]
