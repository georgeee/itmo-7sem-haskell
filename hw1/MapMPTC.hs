{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module MapMPTC (Map(..), BTreeNode(..), btShow) where
import qualified Data.Map as M
import Prelude hiding (lookup, foldl, foldr)
import qualified Prelude as P

class Map k v t | t -> k, t -> v where
  empty :: t
  insert :: k -> v -> t -> t
  delete :: k -> t -> t
  delete = update (const Nothing)
  lookup :: k -> t -> Maybe v
  update :: (v -> Maybe v) -> k -> t -> t
  update f k m = case lookup k m of
                   Nothing -> m
                   Just x -> case f x of
                               Nothing -> delete k m
                               Just x' -> (insert k x' . delete k) m
  adjust :: (v -> v) -> k -> t -> t
  adjust f = update $ Just . f

  fromList :: [(k, v)] -> t
  fromList = P.foldl (flip $ uncurry insert) empty

  foldl :: (b -> kv -> b) -> b -> t -> b
  foldl =

class BTreeNode k n | n -> k where
  btNodeKey :: n -> k
  btLeft :: n -> n
  btRight :: n -> n
  btAddInfo :: n -> String
  btIsEmpty :: n -> Bool

btShow :: (Show k, BTreeNode k n) => n -> String
btShow = foldl1 (\xs x -> (xs ++ "\n" ++ x)) . treeIndent
  where treeIndent node |btIsEmpty node = ["-- /-"]
                        |otherwise      =
          ["-- (" ++ (show $ btNodeKey node) ++ ", " ++ (btAddInfo node) ++ ")"] ++
          (if (not $ btIsEmpty $ btLeft node) then map ("  |" ++) ls else []) ++
          (if (not $ btIsEmpty $ btLeft node) || (not $ btIsEmpty $ btRight node) then ("  `" ++ r) : map ("   " ++) rs else [])
          where
            (r:rs) = treeIndent $ btRight node
            ls     = treeIndent $ btLeft node


