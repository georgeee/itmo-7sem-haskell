module DefaultInstancesStrict where
import Map
import qualified Data.Map.Strict as SM

instance Ord k => Map k v (SM.Map k v) where
  empty = SM.empty
  insert = SM.insert
  delete = SM.delete
  lookup = SM.lookup
  update = SM.update
  adjust = SM.adjust
  fromList = SM.fromList
  foldrWithKey = SM.foldrWithKey
  foldlWithKey = SM.foldlWithKey
  foldr = SM.foldr
  foldl = SM.foldl
  toList = SM.toList

