{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module DefaultInstancesLazy where
import Map
import qualified Data.Map.Lazy as LM

instance Ord k => Map k v (LM.Map k v) where
  empty = LM.empty
  isEmpty = LM.null
  insert = LM.insert
  delete = LM.delete
  lookup = LM.lookup
  update = LM.update
  adjust = LM.adjust
  fromList = LM.fromList
  foldrWithKey = LM.foldrWithKey
  foldlWithKey = LM.foldlWithKey
  foldr = LM.foldr
  foldl = LM.foldl
  toList = LM.toList
  lookup' k n = (LM.lookupLT k n, LM.lookup k n >>= return . \v -> (k, v), LM.lookupGT k n)

