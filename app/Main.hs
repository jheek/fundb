module Main where

import Lib
-- import qualified Data.Map.Strict as Map
-- import Data.IORef
-- import Control.Concurrent.QSem
-- import Control.Monad
-- import Data.Compact
-- import qualified Control.Exception as Exception
-- import qualified Control.Monad as Monad
-- import qualified Data.ByteString as ByteString
-- import qualified Data.Map.Strict as Map


-- data CRef a = CRef {_root :: {- UNPACK -} !(IORef a), _cr :: {- UNPACK -}!(IORef (Compact a)), _compactLock :: {- UNPACK -} !QSem}

-- newCRef :: a -> IO (CRef a)
-- newCRef val = do
--   cr <- compact val
--   rootRef <- newIORef (getCompact cr)
--   crRef <- newIORef cr
--   lock <- newQSem 1
--   return $! CRef { _root = rootRef, _cr = crRef, _compactLock = lock}

-- withLock :: CRef b -> IO a -> IO a
-- withLock (CRef {_compactLock = lock}) action = do
--   waitQSem lock
--   result <- action
--   signalQSem lock
--   return result

-- readCRef :: CRef a -> IO a
-- readCRef ref @ CRef {_root = rootRef} = withLock ref (readIORef rootRef)

-- writeCRef :: CRef a -> a -> IO ()
-- writeCRef ref @ CRef {_root = rootRef} newVal = withLock ref (writeIORef rootRef newVal)

-- {- INLINE modifyCRef -}
-- modifyCRef :: CRef a -> (a -> IO (Bool, a)) -> IO ()
-- modifyCRef ref @ CRef {_root = rootRef, _cr = crRef} modifier = withLock ref $ do
--   root <- readIORef rootRef
--   (shouldCompact, newRoot) <- modifier root
--   if shouldCompact then do
--     cr <- readIORef crRef
--     newCr <- compactAdd cr newRoot
--     writeIORef rootRef (getCompact newCr)
--     writeIORef crRef newCr
--   else writeIORef rootRef newRoot

-- compactCRef :: CRef a -> IO ()
-- compactCRef ref @ CRef {_root = rootRef, _cr = crRef} = withLock ref $ do
--   root <- readIORef rootRef
--   cr <- readIORef crRef
--   newCr <- compactAdd cr root
--   writeIORef rootRef (getCompact newCr)
--   writeIORef crRef newCr

-- recompactCRef :: CRef a -> IO ()
-- recompactCRef ref @ CRef {_root = rootRef, _cr = crRef} = withLock ref $ do
--   root <- readIORef rootRef
--   newCr <- compact root
--   writeIORef rootRef (getCompact newCr)
--   writeIORef crRef newCr

-- class Compactable a where
--   share :: a -> a -> a

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

-- bigMap :: Int -> IO (Map.Map Int Int)
-- bigMap n =
--   return $ go Map.empty 0
--   where 
--     go m i
--       | i < n && Map.size m <  3000000 = go (Map.insert i i m) (i + 1)
--       | i < n && Map.size m >= 3000000 = go (Map.deleteAt 0 m) i
--       | otherwise = m

main :: IO ()
-- main = do
--     m <- bigCMap 5000000
--     print (Map.size m)

main = mapMain



 