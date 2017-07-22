module Main where

import Map

main :: IO ()
main = do
    let m = makeSet ([1..benchSize] :: [Int])
    print $ findMin m 

-- import qualified Data.Map.Strict as Map

-- main :: IO ()
-- main = do
--   let go m i
--         | i <= benchSize = go (Map.insert i () m) (i + 1)
--         | otherwise = m
--   let filledMap = go Map.empty 1
--   print $ Map.findMin filledMap

benchSize :: Int
benchSize = 1000000

-- import qualified Data.Map.Strict as Map

-- {- INLINE bigCMap -}
-- bigCMap :: Int -> IO (Map.Map Int Int)
-- bigCMap n = do
--   cmap <- newCRef Map.empty
--   let go i = modifyCRef cmap $ \m -> do
--         let tm = if (Map.size m >= 3000000) 
--               then (Map.deleteAt 0 m) 
--               else m
--         let inserted = Map.insert i i tm
--         return (i `mod` 100 == 0, inserted)
--   mapM_ go [1..n]
--   readCRef cmap