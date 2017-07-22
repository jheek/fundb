{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Edward Kmett 2015
-- License     : BSD-style
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Portability : non-portable
--
-- Small primitive boxed arrays
--
--------------------------------------------------------------------------------
module SmallArray (
  SmallArray(..), SmallMutableArray(..),
  newSmallArray, emptySmallArray, readSmallArray, writeSmallArray, indexSmallArray, indexSmallArrayM,
  unsafeFreezeSmallArray, unsafeThawSmallArray, sameSmallMutableArray,
  copySmallArray, copySmallMutableArray,
  cloneSmallArray, cloneSmallMutableArray,
  insertSmallArray, insertSmallMutableArray, replaceSmallArray, bsearch, bsearch', SearchIndex(..),
  removeSmallArray, removeSmallMutableArray,
  sizeOfSmallArray, sizeOfSmallMutableArray
) where

import Control.DeepSeq
import Control.Monad.Primitive
import Data.Foldable as Foldable
import GHC.Exts
import GHC.ST

-- | Boxed arrays
data SmallArray a = SmallArray (SmallArray# a)

-- | Mutable boxed arrays associated with a primitive state token.
data SmallMutableArray s a = SmallMutableArray (SmallMutableArray# s a)

emptySmallArray :: SmallArray a
emptySmallArray = runST $ do
  mut <- newSmallArray 0 undefined
  unsafeFreezeSmallArray mut

-- | Create a new mutable array of the specified size and initialise all
-- elements with the given value.
newSmallArray :: PrimMonad m => Int -> a -> m (SmallMutableArray (PrimState m) a)
{-# INLINE newSmallArray #-}
newSmallArray (I# n#) x = primitive
   (\s# -> case newSmallArray# n# x s# of
             (# s'#, arr# #) -> (# s'#, SmallMutableArray arr# #))

-- | Read a value from the array at the given index.
readSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> m a
{-# INLINE readSmallArray #-}
readSmallArray (SmallMutableArray arr#) (I# i#) = primitive (readSmallArray# arr# i#)

-- | Write a value to the array at the given index.
writeSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> a -> m ()
{-# INLINE writeSmallArray #-}
writeSmallArray (SmallMutableArray arr#) (I# i#) x = primitive_ (writeSmallArray# arr# i# x)

-- | Read a value from the immutable array at the given index.
indexSmallArray :: SmallArray a -> Int -> a
{-# INLINE indexSmallArray #-}
indexSmallArray (SmallArray arr#) (I# i#) = case indexSmallArray# arr# i# of (# x #) -> x


-- | Monadically read a value from the immutable array at the given index.
-- This allows us to be strict in the array while remaining lazy in the read
-- element which is very useful for collective operations. Suppose we want to
-- copy an array. We could do something like this:
--
-- > copy marr arr ... = do ...
-- >                        writeSmallArray marr i (indexSmallArray arr i) ...
-- >                        ...
--
-- But since primitive arrays are lazy, the calls to 'indexSmallArray' will not be
-- evaluated. Rather, @marr@ will be filled with thunks each of which would
-- retain a reference to @arr@. This is definitely not what we want!
--
-- With 'indexSmallArrayM', we can instead write
--
-- > copy marr arr ... = do ...
-- >                        x <- indexSmallArrayM arr i
-- >                        writeSmallArray marr i x
-- >                        ...
--
-- Now, indexing is executed immediately although the returned element is
-- still not evaluated.
--
indexSmallArrayM :: Monad m => SmallArray a -> Int -> m a
{-# INLINE indexSmallArrayM #-}
indexSmallArrayM (SmallArray arr#) (I# i#)
  = case indexSmallArray# arr# i# of (# x #) -> return x

-- | Convert a mutable array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezeSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> m (SmallArray a)
{-# INLINE unsafeFreezeSmallArray #-}
unsafeFreezeSmallArray (SmallMutableArray arr#)
  = primitive (\s# -> case unsafeFreezeSmallArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, SmallArray arr'# #))

-- | Convert an immutable array to an mutable one without copying. The
-- immutable array should not be used after the conversion.
unsafeThawSmallArray :: PrimMonad m => SmallArray a -> m (SmallMutableArray (PrimState m) a)
{-# INLINE unsafeThawSmallArray #-}
unsafeThawSmallArray (SmallArray arr#)
  = primitive (\s# -> case unsafeThawSmallArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, SmallMutableArray arr'# #))

-- | Check whether the two arrays refer to the same memory block.
sameSmallMutableArray :: SmallMutableArray s a -> SmallMutableArray s a -> Bool
{-# INLINE sameSmallMutableArray #-}
sameSmallMutableArray (SmallMutableArray arr#) (SmallMutableArray brr#)
  = isTrue# (sameSmallMutableArray# arr# brr#)

-- | Copy a slice of an immutable array to a mutable array.
copySmallArray :: PrimMonad m
          => SmallMutableArray (PrimState m) a    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> SmallArray a                         -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
{-# INLINE copySmallArray #-}
copySmallArray (SmallMutableArray dst#) (I# doff#) (SmallArray src#) (I# soff#) (I# len#)
  = primitive_ (copySmallArray# src# soff# dst# doff# len#)

-- | Copy a slice of a mutable array to another array. The two arrays may
-- not be the same.
copySmallMutableArray :: PrimMonad m
          => SmallMutableArray (PrimState m) a    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> SmallMutableArray (PrimState m) a    -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
{-# INLINE copySmallMutableArray #-}
-- NOTE: copySmallArray# and copySmallMutableArray# are slightly broken in GHC 7.6.* and earlier
copySmallMutableArray (SmallMutableArray dst#) (I# doff#)
                 (SmallMutableArray src#) (I# soff#) (I# len#)
  = primitive_ (copySmallMutableArray# src# soff# dst# doff# len#)

-- | Return a newly allocated SmallArray with the specified subrange of the
-- provided SmallArray. The provided SmallArray should contain the full subrange
-- specified by the two Ints, but this is not checked.
cloneSmallArray :: SmallArray a -- ^ source array
           -> Int     -- ^ offset into destination array
           -> Int     -- ^ number of elements to copy
           -> SmallArray a
{-# INLINE cloneSmallArray #-}
cloneSmallArray (SmallArray arr#) (I# off#) (I# len#) 
  = case cloneSmallArray# arr# off# len# of arr'# -> SmallArray arr'#

-- | Return a newly allocated SmallMutableArray. with the specified subrange of
-- the provided SmallMutableArray. The provided SmallMutableArray should contain the
-- full subrange specified by the two Ints, but this is not checked.
cloneSmallMutableArray :: PrimMonad m
        => SmallMutableArray (PrimState m) a -- ^ source array
        -> Int                          -- ^ offset into destination array
        -> Int                          -- ^ number of elements to copy
        -> m (SmallMutableArray (PrimState m) a)
{-# INLINE cloneSmallMutableArray #-}
cloneSmallMutableArray (SmallMutableArray arr#) (I# off#) (I# len#) = primitive
   (\s# -> case cloneSmallMutableArray# arr# off# len# s# of
             (# s'#, arr'# #) -> (# s'#, SmallMutableArray arr'# #))

-- | size of small array
sizeOfSmallArray :: SmallArray a -> Int
{-# INLINE sizeOfSmallArray #-}
sizeOfSmallArray (SmallArray ar) = I# (sizeofSmallArray# ar)

-- | size of small mutable array
sizeOfSmallMutableArray :: (forall m. (SmallMutableArray m a)) -> Int
{-# INLINE sizeOfSmallMutableArray #-}
sizeOfSmallMutableArray (SmallMutableArray ar) = I# (sizeofSmallMutableArray# ar)

-- | insert a value at the give position and returns a mutable array
insertSmallMutableArray :: (PrimMonad m)
  => SmallArray a -- ^ source array
  -> Int -- ^ insert position
  -> a -- ^ insert value
  -> m (SmallMutableArray (PrimState m) a) -- ^ mutable array which is 1 element longer than the source array
{-# INLINE insertSmallMutableArray #-}
insertSmallMutableArray ar ind val = do
  let s = sizeOfSmallArray ar
  mut <- newSmallArray (s + 1) undefined
  copySmallArray mut 0 ar 0 ind
  writeSmallArray mut ind val
  copySmallArray mut (ind+1) ar ind (s-ind)
  return $! mut

-- | insert a value at given index
insertSmallArray :: ()
  => SmallArray a -- ^ source array
  -> Int -- ^ insert position
  -> a -- ^ insert value
  -> SmallArray a
{-# INLINE insertSmallArray #-}
insertSmallArray ar ind val = runST $ do
  mut <- insertSmallMutableArray ar ind val
  unsafeFreezeSmallArray mut

-- | removes a value at the give position and returns a mutable array
removeSmallMutableArray :: (PrimMonad m)
  => SmallArray a -- ^ source array
  -> Int -- ^ remove position
  -> m (SmallMutableArray (PrimState m) a) -- ^ mutable array which is 1 element longer than the source array
{-# INLINE removeSmallMutableArray #-}
removeSmallMutableArray ar ind = do
  let s = sizeOfSmallArray ar
  mut <- newSmallArray (s - 1) undefined
  copySmallArray mut 0 ar 0 ind
  copySmallArray mut ind ar (ind+1) (s - ind - 1)
  return $! mut

-- | remove a value at given index
removeSmallArray :: ()
  => SmallArray a -- ^ source array
  -> Int -- ^ remove position
  -> SmallArray a
{-# INLINE removeSmallArray #-}
removeSmallArray ar ind = runST $ do
  mut <- removeSmallMutableArray ar ind
  unsafeFreezeSmallArray mut 

-- | Result of an index search. 
data SearchIndex = Found {-# UNPACK #-} !Int -- ^ key was found at given index
                 | Lost {-# UNPACK #-} !Int -- ^ key was not found and should be inserted at given index to keep the array ordered

-- | Binary search for given key.
--   Asserts array to be ordered.
bsearch :: Ord k => SmallArray k -> k -> SearchIndex
{-# INLINE bsearch #-}
bsearch ar key = bsearch' ar key 0 ((sizeOfSmallArray ar) - 1)

-- | Binary search for given key between start and end positions.
--   Asserts array to be ordered.
bsearch' :: Ord k 
  => SmallArray k -- ^ source array
  -> k -- ^ search key
  -> Int -- ^ start position (inclusive)
  -> Int -- ^ end position (inclusive)
  -> SearchIndex
{-# INLINE bsearch' #-}
bsearch' ar key start end
  | start > end = Lost start
  | otherwise   = case key `compare` piv of
    LT -> bs start (mid - 1)
    GT -> bs (mid + 1) end
    EQ -> Found mid
  where 
    mid = (start + end) `div` 2
    piv = indexSmallArray ar mid
    bs = bsearch' ar key

-- | replace a single value
replaceSmallArray :: 
     SmallArray a  -- ^ source array
  -> Int -- ^ position
  -> a  -- ^ replacement
  -> SmallArray a
{-# INLINE replaceSmallArray #-}
replaceSmallArray ar ind val = runST $ do
  let s = sizeOfSmallArray ar
  let clone = cloneSmallArray ar 0 s
  mut <- unsafeThawSmallArray clone
  writeSmallArray mut ind val
  unsafeFreezeSmallArray mut

instance IsList (SmallArray a) where
  type Item (SmallArray a) = a
  toList = Foldable.toList
  fromListN n xs0 = runST $ do
    arr <- newSmallArray n undefined
    let go !_ []     = return ()
        go k (x:xs) = writeSmallArray arr k x >> go (k+1) xs
    go 0 xs0
    unsafeFreezeSmallArray arr
  fromList xs = fromListN (Prelude.length xs) xs
  {-# INLINE toList #-}
  {-# INLINE fromListN #-}
  {-# INLINE fromList #-}

instance Functor SmallArray where
  {-# INLINE fmap #-}
  fmap f !i = runST $ do
    let n = length i
    o <- newSmallArray n undefined
    let go !k
          | k == n = return ()
          | otherwise = do
            a <- indexSmallArrayM i k
            writeSmallArray o k (f a)
            go (k+1)
    go 0
    unsafeFreezeSmallArray o

instance Foldable SmallArray where
  foldr f z arr = go 0 where
    n = length arr
    go !k
      | k == n    = z
      | otherwise = f (indexSmallArray arr k) (go (k+1))

  foldl f z arr = go (length arr - 1) where
    go !k
      | k < 0 = z
      | otherwise = f (go (k-1)) (indexSmallArray arr k)

  foldr' f z arr = go 0 where
    n = length arr
    go !k
      | k == n    = z
      | r <- indexSmallArray arr k = r `seq` f r (go (k+1))

  foldl' f z arr = go (length arr - 1) where
    go !k
      | k < 0 = z
      | r <- indexSmallArray arr k = r `seq` f (go (k-1)) r

  length (SmallArray ary) = I# (sizeofSmallArray# ary)
  {-# INLINE length #-}
  {-# INLINE foldr #-}
  {-# INLINE foldr' #-}
  {-# INLINE foldl #-}
  {-# INLINE foldl' #-}

instance Traversable SmallArray where
  traverse f a = fromListN (length a) <$> traverse f (Foldable.toList a)

instance Show a => Show (SmallArray a) where
  showsPrec d as = showParen (d > 10) $
    showString "fromList " . showsPrec 11 (Foldable.toList as)

instance Read a => Read (SmallArray a) where
  readsPrec d = readParen (d > 10) $ \s -> [(fromList m, u) | ("fromList", t) <- lex s, (m,u) <- readsPrec 11 t]

instance Ord a => Ord (SmallArray a) where
  compare as bs = compare (Foldable.toList as) (Foldable.toList bs)

instance Eq a => Eq (SmallArray a) where
  as == bs = Foldable.toList as == Foldable.toList bs

instance NFData a => NFData (SmallArray a) where
  rnf a0 = go a0 (length a0) 0 where
    go !a !n !i
      | i >= n = ()
      | otherwise = rnf (indexSmallArray a i) `seq` go a n (i+1)
  {-# INLINE rnf #-}