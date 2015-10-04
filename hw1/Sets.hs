{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             FunctionalDependencies, FlexibleContexts #-}
module Sets where
import Prelude hiding (lookup, foldl, foldr, min, max)
import qualified Prelude as P
import qualified Map as M
import qualified Set as S
import DummyMap
import LLRB
import Data.Monoid

data MapSet t k = MapSet { msHolder :: t }

instance (Show k, Show t) => Show (MapSet t k) where
  show = show . msHolder

instance (M.Map k () t) => S.Set k (MapSet t k) where
  empty = MapSet $ M.empty
  isEmpty = M.isEmpty . msHolder
  insert k = MapSet . M.insert k () . msHolder
  delete k = MapSet . M.delete k . msHolder
  fromList = MapSet . M.fromList . flip zip (repeat ())
  foldr f h = M.foldrWithKey (flip $ const f) h . msHolder
  lookup' k = (\(a, b, c) -> (m a, m b, m c)) . M.lookup' k . msHolder
    where m = (>>= return . fst)

instance (Monoid t) => Monoid (MapSet t k) where
  mempty = MapSet mempty
  mappend a b = MapSet $ mappend (msHolder a) (msHolder b)

type DummySet k = MapSet (DummyMap k ()) k
type LLRBTreeSet k = MapSet (LLRBTreeMap k ()) k
