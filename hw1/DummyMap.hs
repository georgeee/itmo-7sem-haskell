{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module DummyMap (DummyMap) where
import Prelude hiding (lookup)
import Map as M
import Data.Monoid

data DummyMap k v = Empty | Node { key::k, value::v, left::(DummyMap k v), right::(DummyMap k v) }

instance BSTMap k v (DummyMap k v) where
  btKey = key
  btValue = value
  btLeft = left
  btRight = right
  btAddInfo = const ""
  btIsEmpty Empty = True
  btIsEmpty _ = False

instance (Show k, Show v) => Show (DummyMap k v) where
  show = btShow

instance (Ord k) => Map k v (DummyMap k v) where
  empty = Empty
  insert k v Empty = Node k v Empty Empty
  insert k v (Node k' v' l r) | k == k' = Node k v l r
                              | k < k' = Node k' v' (ins l) r
                              | k > k' = Node k' v' l (ins r)
      where ins = insert k v
  delete _ Empty = Empty
  delete k n@(Node k' v' l r) | k == k' = merge l r
                              | k < k' = n { left = delete k l }
                              | k > k' = n { right = delete k r }
  foldrWithKey = btFoldrWithKey
  lookup' = btLookup'
  isEmpty = btIsEmpty

instance (Ord k) => Monoid (DummyMap k v) where
  mempty = empty
  mappend = btAppend

merge :: (Ord k) => (DummyMap k v) -> (DummyMap k v) -> DummyMap k v
merge Empty n = n
merge n Empty = n
merge (Node k v l Empty) r = Node k v l r
merge l r = Node k v l' r
  where (l', Node k v Empty Empty) = pickupR l
        pickupR (Node pk pv pl n@(Node nk nv nl Empty)) = (Node pk pv pl nl, Node nk nv Empty Empty)
        pickupR p@(Node pk pv pl n@(Node _ _ _ nr)) = let (n', res) = pickupR n
                                                       in (Node pk pv pl n', res)

