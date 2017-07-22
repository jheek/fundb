{-# LANGUAGE NamedFieldPuns, StandaloneDeriving, OverloadedLists, BangPatterns #-}

--------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Jonathan Heek 2017
-- License     : BSD-style
-- Maintainer  : Jonathan Heek <heekjonathan@gmail.com>
-- Portability : non-portable
--
-- Efficient Map implementation using B-Tree and GHC's SmallArrays
--
--------------------------------------------------------------------------------
module Map
  ( Map(..)
  , insert
  , insertMany
  , addMany
  , empty
  , bsearch
  , makeMap
  , makeSet
  , entries
  , findMin
  , mapKeys
  , mapValues
  , width
  , split, splitL, splitR
  ) where

import SmallArray
import Control.Monad.ST
import Data.Foldable (Foldable, foldl', toList)

-- | Map data structure
data Map k v 
  = Leave 
    { keys :: {-# UNPACK #-} !(SmallArray k)
    , vals :: {-# UNPACK #-} !(SmallArray v) } -- ^ A leave contains an ordered array of keys and values
  | Node
    { keys   :: {-# UNPACK #-} !(SmallArray k)
    , vals   :: {-# UNPACK #-} !(SmallArray v)
    , childs :: {-# UNPACK #-} !(SmallArray (Map k v)) } -- ^ A node contains an ordered array of keys and values as well as child trees

type Set a = Map a ()

deriving instance (Show k, Show v) => Show (Map k v)
deriving instance (Eq k, Eq v) => Eq (Map k v)

-- | number of entries in a single Leave or Node of the Map
width :: Int
{-# INLINE width #-}
width = 32



-- | empty map
empty :: Map k v
empty = Leave {keys = emptySmallArray, vals = emptySmallArray}

-- | insert a list of entries into the map
insertMany :: (Ord k, Foldable f) => Map k v -> f (k, v) -> Map k v
{-# INLINE insertMany #-}
insertMany = foldl' (\m (key, val) -> insert m key val)

-- | insert a list of items into the set
addMany :: (Ord a, Foldable f) => Set a -> f a -> Set a
{-# INLINE addMany #-}
addMany = foldl' (\m item -> insert m item ())

-- | create a new map given a list of entries
makeMap :: (Ord k, Foldable f) => f (k, v) -> Map k v
{-# INLINE makeMap #-}
makeMap = insertMany empty

makeSet :: (Ord k, Foldable f) => f k -> Map k ()
{-# INLINE makeSet #-}
makeSet = addMany empty 

-- | in-order list of entries for given map
entries :: Map k v -> [(k, v)]
{-# INLINE entries #-}
entries Leave {keys, vals} = zip (toList keys) (toList vals)
entries Node {keys, vals, childs} = concat $ interleave nodeEntries childEntries
  where 
    childEntries = map entries (toList childs)
    nodeEntries = map (:[]) $ zip (toList keys) (toList vals)

findMin :: Map k v -> (k, v)
findMin (Node {childs}) = findMin (indexSmallArray childs 0)
findMin (Leave {keys, vals}) = (indexSmallArray keys 0, indexSmallArray vals 0)

mapKeys :: Map k v -> [k]
mapKeys = map fst . entries

mapValues :: Map k v -> [v]
mapValues = map snd . entries


-- | interleaves two lists as (y1,x1,y2,x2,y3...)
interleave :: ()
  => [a] -- ^ xs
  -> [a] -- ^ ys
  -> [a]
{-# INLINE interleave #-}
interleave (x:xs) (y:yy:ys) = y : x : (interleave xs (yy:ys))
interleave _ [y] = [y]
interleave _ [] = []
interleave [] (_:_:_) = error "Interleaved list is too short"


-- data ModNode k v 
--   = UnaryNode !(Map k v) 
--   | BinNode !k !v !(Map k v) !(Map k v)
--   | NoMod

-- -- | Modifier
-- type Modifier v = (Maybe v -> Maybe v)

-- unpackModNode :: Map k v -> InsNode k v -> Map k v
-- unpackModNode _ (UnaryNode m) = m
-- unpackModNode _ (BinNode k v left right) = Node {keys = [k], vals = [v], childs = [left, right]}
-- unpackModNode m NoMod = m

-- modify :: (Ord k)
--   => Modifier v
--   -> Map k v
--   -> k
--   -> Map k v
-- modify m modifier key = unpackModNode $ modify' m modifier key

-- modify' :: (Ord k)
--   => Modifier v
--   -> Map k v
--   -> k
--   -> ModNode k v
-- modify' m modifier key = case bsearch _keys key of
--   Found ind -> case m of
--     Leave {} -> UnaryNode $ Leave {keys = _keys, vals = rvals}
--     Node {childs} -> UnaryNode $ Node {keys = _keys, vals = rvals, childs = childs}
--     where
--       val   = indexSmallArray _vals ind
--       next  = modifier (Just val)
--       (rkeys, rvals) = case next of
--         Just newV -> (_keys, replaceSmallArray _vals ind newV)
--         Nothing   -> (removeSmallArray _keys ind, removeSmallArray _vals ind)
--   Lost ind -> case m of
--     Leave {} ->  case w < width of
--       True  -> UnaryNode $ Leave { keys = ikeys, vals = ivals }
--       False -> BinNode pkey pval left right
--       where
--         left = Leave { keys = lkeys, vals = lvals }
--         right = Leave { keys = rkeys, vals = rvals }
--         ikeys = insertSmallArray _keys ind key
--         ivals = insertSmallArray _vals ind val
--         pkey  = indexSmallArray ikeys halfw
--         pval  = indexSmallArray ivals halfw
--         lkeys = cloneSmallArray ikeys 0 halfw
--         rkeys = cloneSmallArray ikeys (halfw + 1) halfw
--         lvals = cloneSmallArray ivals 0 halfw
--         rvals = cloneSmallArray ivals (halfw + 1) halfw
--     Node {} -> case insert' child key val of
--       UnaryNode n -> UnaryNode$ Node { keys = _keys, vals = _vals, childs = replaceSmallArray _childs ind n }
--       BinNode k v left right -> case w < width of
--         True  -> UnaryNode $ Node { keys = ikeys, vals = ivals, childs = ichilds }
--         False -> BinNode pkey pval nleft nright
--         where
--           ikeys = insertSmallArray _keys ind k
--           ivals = insertSmallArray _vals ind v
--           ichilds = runST $ do
--             mut <- insertSmallMutableArray _childs ind left
--             writeSmallArray mut (ind + 1) right
--             unsafeFreezeSmallArray mut
--           pkey = indexSmallArray ikeys halfw
--           pval = indexSmallArray ivals halfw
--           side f = Node 
--             { keys = cloneSmallArray ikeys f halfw
--             , vals = cloneSmallArray ivals f halfw
--             , childs = cloneSmallArray ichilds f (halfw + 1) }
--           nleft = side 0
--           nright = side (halfw + 1)
--     where
--       child = indexSmallArray _childs ind
--       w     = sizeOfSmallArray _keys
--       halfw = width `div` 2
--   where 
--     _keys = keys m
--     _vals = vals m
--     _childs = childs m

-- data Zipper k v 
--   = Zipper { zipStart :: Int, zipEnd :: Int, zipMap :: Map k v, zipPath :: [Map k v]}

-- zipFind :: Ord k => Zipper k v -> k -> Zipper k v
-- zipFind (Zipper {zipStart, zipEnd, zipMap, zipPath}) key = case bsearch' (keys zipMap) k zipStart zipEnd of
--   Found ind -> Zipper {zipStart = ind, zipEnd = index, zipMap = zipMap, zipPath = zipPath}
--   Lost  ind -> Zipper {zipStart = }

-- | splits a map into two maps based on a seperator key
split :: (Ord k) => k -> Map k v -> (Map k v, Map k v)
{-# INLINE split #-}
split !pivot !tree = case tree of
  Leave {} -> case pivS of
    Found ind -> (splitLeave 0 ind, splitLeave (ind + 1) (w - ind - 1))
    Lost  ind -> (splitLeave 0 ind, splitLeave ind (w - ind))
    where
      splitLeave i l = Leave {keys = cloneSmallArray _keys i l, vals = cloneSmallArray _vals i l}
  Node {childs} -> case pivS of
    Found ind -> (splitNode 0 ind, splitNode (ind + 1) (w - ind - 1))
      where
        splitNode i l = Node {keys = cloneSmallArray _keys i l, vals = cloneSmallArray _vals i l, childs = cloneSmallArray childs i (l + 1)}
    Lost  ind -> (splitNode 0 ind lchilds, splitNode ind (w - ind) rchilds)
      where 
        splitNode i l c = Node {keys = cloneSmallArray _keys i l, vals = cloneSmallArray _vals i l, childs = c}
        (edgeL, edgeR) = split pivot (indexSmallArray childs ind)
        lchilds = runST $ do
          mut <- newSmallArray (ind + 1) undefined
          copySmallArray mut 0 childs 0 ind
          writeSmallArray mut ind edgeL
          unsafeFreezeSmallArray mut
        rchilds = runST $ do
          let l = w - ind + 1
          mut <- newSmallArray l undefined
          copySmallArray mut 1 childs (ind + 1) (l - 1)
          writeSmallArray mut 0 edgeR 
          unsafeFreezeSmallArray mut
  where 
    _keys = keys tree
    _vals = vals tree
    w = sizeOfSmallArray _keys
    pivS = bsearch _keys pivot

-- | returns a map with all elements that are less than the given key
splitL :: (Ord k) => k -> Map k v -> Map k v
splitL !piv !m = case split piv m of (left, _) -> left

-- | returns a map with all elements that are greater than the given key
splitR :: (Ord k) => k -> Map k v -> Map k v
splitR !piv !m = case split piv m of (_, right) -> right

-- union :: Map k v -> Map k v -> Map k v


data InsNode k v = UnaryNode !(Map k v) | BinNode !k !v !(Map k v) !(Map k v)

unpackInsNode :: InsNode k v -> Map k v
{-# INLINE unpackInsNode #-}
unpackInsNode (UnaryNode m) = m
unpackInsNode (BinNode k v left right) = Node {keys = [k], vals = [v], childs = [left, right]}

-- | insert an entry into the map
insert :: Ord k => Map k v -> k -> v -> Map k v 
{-# INLINE insert #-}
insert m key val = unpackInsNode (insert' m key val)

insert' :: Ord k => Map k v -> k -> v -> InsNode k v
{-# INLINE insert' #-}
insert' m key val = case bsearch _keys key of
  Found ind -> case m of
    Leave {} -> UnaryNode $ Leave {keys = rkeys, vals = rvals}
    Node {childs} -> UnaryNode $ Node {keys = rkeys, vals = rvals, childs = childs}
    where
      rkeys = replaceSmallArray _keys ind key
      rvals = replaceSmallArray _vals ind val
  Lost ind -> case m of
    Leave {} ->  case w < width of
      True  -> UnaryNode $ Leave { keys = ikeys, vals = ivals }
      False -> BinNode pkey pval left right
      where
        left = Leave { keys = lkeys, vals = lvals }
        right = Leave { keys = rkeys, vals = rvals }
        ikeys = insertSmallArray _keys ind key
        ivals = insertSmallArray _vals ind val
        pkey  = indexSmallArray ikeys halfw
        pval  = indexSmallArray ivals halfw
        lkeys = cloneSmallArray ikeys 0 halfw
        rkeys = cloneSmallArray ikeys (halfw + 1) halfw
        lvals = cloneSmallArray ivals 0 halfw
        rvals = cloneSmallArray ivals (halfw + 1) halfw
    Node {} -> case insert' child key val of
      UnaryNode n -> UnaryNode$ Node { keys = _keys, vals = _vals, childs = replaceSmallArray _childs ind n }
      BinNode k v left right -> case w < width of
        True  -> UnaryNode $ Node { keys = ikeys, vals = ivals, childs = ichilds }
        False -> BinNode pkey pval nleft nright
        where
          ikeys = insertSmallArray _keys ind k
          ivals = insertSmallArray _vals ind v
          ichilds = runST $ do
            mut <- insertSmallMutableArray _childs ind left
            writeSmallArray mut (ind + 1) right
            unsafeFreezeSmallArray mut
          pkey = indexSmallArray ikeys halfw
          pval = indexSmallArray ivals halfw
          side f = Node 
            { keys = cloneSmallArray ikeys f halfw
            , vals = cloneSmallArray ivals f halfw
            , childs = cloneSmallArray ichilds f (halfw + 1) }
          nleft = side 0
          nright = side (halfw + 1)
    where
      child = indexSmallArray _childs ind
      w     = sizeOfSmallArray _keys
      halfw = width `div` 2
  where 
    _keys = keys m
    _vals = vals m
    _childs = childs m
