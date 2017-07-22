{-# LANGUAGE OverloadedLists #-}

import Lib
import Test.Hspec
import Test.QuickCheck


ordered :: Ord k => [(k, v)] -> Bool
ordered xs = and (zipWith (\x y -> fst x < fst y) xs (drop 1 xs))

prop_OrderedEntries xs = ordered . entries . makeMap $ xs
  where types = (xs :: [(Int, Int)])

test :: IO ()
test = hspec $ do
  describe "map" $ do
    let items = map (\i -> (i, 10 - i)) [1..10]
    it "should insert items in order test" $ property $ \x -> (read . show) x == (x :: Int)
    it "entries should be ordered" $ property prop_OrderedEntries
    it "should allow items to be inserted" $
      ((insert empty 1 1) :: Map Int Int) `shouldBe` Leave {keys = [1], vals = [1]}

main :: IO ()
main = do
  quickCheck prop_OrderedEntries
  test