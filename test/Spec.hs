{-# LANGUAGE OverloadedLists #-}

import Lib
import Test.Hspec
import Test.QuickCheck


ordered :: Ord k => [(k, v)] -> Bool
ordered xs = and (zipWith (\x y -> fst x < fst y) xs (drop 1 xs))

prop_OrderedEntries :: [(Int, Int)] -> Bool
prop_OrderedEntries xs = ordered . entries . makeMap $ xs

test :: IO ()
test = hspec $ do
  describe "map" $ do
    it "entries should be ordered" $ property prop_OrderedEntries
    it "should allow items to be inserted" $
      ((insert empty 1 1) :: Map Int Int) `shouldBe` Leave {keys = [1], vals = [1]}

main :: IO ()
main = do
  quickCheck prop_OrderedEntries
  test