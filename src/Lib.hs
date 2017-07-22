{-# LANGUAGE NamedFieldPuns, StandaloneDeriving, OverloadedLists #-}

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
module Lib
  ( Map(..)
  , insert
  , insertMany
  , empty
  , bsearch
  , makeMap
  , entries
  , width
  ) where

import SmallArray
import Control.Monad.ST
import Data.Foldable (Foldable, foldl', toList)

data Map k v = Leave {keys :: SmallArray k, vals :: SmallArray v}
             | Node  {keys :: SmallArray k, vals :: SmallArray v, childs :: SmallArray (Map k v)}

deriving instance (Show k, Show v) => Show (Map k v)
deriving instance (Eq k, Eq v) => Eq (Map k v)

-- | number of entries in a single Leave or Node of the Map
width :: Int
width = 2

empty :: Map k v
empty = Leave {keys = [], vals = []}

data InsNode k v = UnaryNode !(Map k v) | BinNode !k !v !(Map k v) !(Map k v)

unpackInsNode :: InsNode k v -> Map k v
unpackInsNode (UnaryNode m) = m
unpackInsNode (BinNode k v left right) = Node {keys = [k], vals = [v], childs = [left, right]}

-- | insert an entry into the map
insert :: Ord k => Map k v -> k -> v -> Map k v 
insert m key val = unpackInsNode (insert' m key val)

-- | insert a list of entries into the map
insertMany :: (Ord k, Foldable f) => Map k v -> f (k, v) -> Map k v
insertMany = foldl' (\m (key, val) -> insert m key val)

-- | create a new map given a list of entries
makeMap :: (Ord k, Foldable f) => f (k, v) -> Map k v
makeMap = insertMany empty

-- | in-order list of entries for given map
entries :: Map k v -> [(k, v)]
entries Leave {keys, vals} = zip (toList keys) (toList vals)
entries Node {keys, vals, childs} = concat $ interleave nodeEntries childEntries
  where 
    childEntries = map entries (toList childs)
    nodeEntries = map (:[]) $ zip (toList keys) (toList vals)

-- | interleaves two lists as (y1,x1,y2,x2,y3...)
interleave :: ()
  => [a] -- ^ xs
  -> [a] -- ^ ys
  -> [a]
interleave (x:xs) (y:yy:ys) = y : x : (interleave xs (yy:ys))
interleave _ [y] = [y]
interleave _ [] = []
interleave [] (_:_:_) = error "Interleaved list is too short"

insert' :: Ord k => Map k v -> k -> v -> InsNode k v
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
