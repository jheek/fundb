
--------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Jonathan Heek 2017
-- License     : BSD-style
-- Maintainer  : Jonathan Heek <heekjonathan@gmail.com>
-- Portability : non-portable
--
-- CRef
--
--------------------------------------------------------------------------------
module CRef
  ( CRef
  , newCRef
  , readCRef
  , writeCRef
  , modifyCRef
  , compactCRef
  , recompactCRef
  ) where

import Data.Compact
import Data.IORef
import Control.Concurrent.QSem

data CRef a = CRef {_root :: {- UNPACK -} !(IORef a), _cr :: {- UNPACK -}!(IORef (Compact a)), _compactLock :: {- UNPACK -} !QSem}

newCRef :: a -> IO (CRef a)
newCRef val = do
  cr <- compact val
  rootRef <- newIORef (getCompact cr)
  crRef <- newIORef cr
  lock <- newQSem 1
  return $! CRef { _root = rootRef, _cr = crRef, _compactLock = lock}

withLock :: CRef b -> IO a -> IO a
withLock (CRef {_compactLock = lock}) action = do
  waitQSem lock
  result <- action
  signalQSem lock
  return result

readCRef :: CRef a -> IO a
readCRef ref @ CRef {_root = rootRef} = withLock ref (readIORef rootRef)

writeCRef :: CRef a -> a -> IO ()
writeCRef ref @ CRef {_root = rootRef} newVal = withLock ref (writeIORef rootRef newVal)

{- INLINE modifyCRef -}
modifyCRef :: CRef a -> (a -> IO (Bool, a)) -> IO ()
modifyCRef ref @ CRef {_root = rootRef, _cr = crRef} modifier = withLock ref $ do
  root <- readIORef rootRef
  (shouldCompact, newRoot) <- modifier root
  if shouldCompact then do
    cr <- readIORef crRef
    newCr <- compactAdd cr newRoot
    writeIORef rootRef (getCompact newCr)
    writeIORef crRef newCr
  else writeIORef rootRef newRoot

compactCRef :: CRef a -> IO ()
compactCRef ref @ CRef {_root = rootRef, _cr = crRef} = withLock ref $ do
  root <- readIORef rootRef
  cr <- readIORef crRef
  newCr <- compactAdd cr root
  writeIORef rootRef (getCompact newCr)
  writeIORef crRef newCr

recompactCRef :: CRef a -> IO ()
recompactCRef ref @ CRef {_root = rootRef, _cr = crRef} = withLock ref $ do
  root <- readIORef rootRef
  newCr <- compact root
  writeIORef rootRef (getCompact newCr)
  writeIORef crRef newCr