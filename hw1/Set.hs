{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Set where
import Prelude hiding (lookup, foldl, foldr)
import qualified Prelude as P
import Data.Maybe (isJust)

class Set k t | t -> k where
  empty :: t
  isEmpty :: t -> Bool
  insert :: k -> t -> t
  delete :: k -> t -> t

  fromList :: [k] -> t
  fromList = P.foldr insert empty

  foldl :: (b -> k -> b) -> b -> t -> b
  foldl f h = P.foldl f h . toList

  foldr :: (k -> b -> b) -> b -> t -> b

  toList :: t -> [k]
  toList = foldr (:) []

  lookup' :: k -> t -> (Maybe k, Maybe k, Maybe k)

  next :: k -> t -> Maybe k
  next k n = let (_, _, next) = lookup' k n in next

  prev :: k -> t -> Maybe k
  prev k n = let (prev, _, _) = lookup' k n in prev

  lookup :: k -> t -> Bool
  lookup k n = let (_, cur, _) = lookup' k n in isJust cur
