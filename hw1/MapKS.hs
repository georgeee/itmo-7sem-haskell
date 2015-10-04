{-# LANGUAGE KindSignatures #-}
module Map (Map(..), BTreeNode(..), btShow) where
import qualified Data.Map as M
import Prelude hiding (lookup, foldl, foldr)
import qualified Data.Foldable as F
import qualified Prelude as P

class Map (t :: * -> * -> *) where
  empty :: t k v
  insert :: k -> v -> t k v -> t k v
  delete :: k -> t k v -> t k v
  delete = update (const Nothing)
  lookup :: k -> t k v -> Maybe v
  update :: (v -> Maybe v) -> k -> t k v -> t k v
  update f k m = case lookup k m of
                   Nothing -> m
                   Just x -> case f x of
                               Nothing -> delete k m
                               Just x' -> (insert k x' . delete k) m
  adjust :: (v -> v) -> k -> t k v -> t k v
  adjust f = update $ Just . f

  fromList :: [(k, v)] -> t k v
  fromList = P.foldl (flip $ uncurry insert) empty

class BTreeNode (n :: * -> *) where
  btNodeKey :: n k -> k
  btLeft :: n k -> n k
  btRight :: n k -> n k
  btAddInfo :: n k -> String
  btIsEmpty :: n k -> Bool

btShow :: (Show k, BTreeNode n) => n k -> String
btShow = foldl1 (\xs x -> (xs ++ "\n" ++ x)) . treeIndent
  where treeIndent node |btIsEmpty node = ["-- /-"]
                        |otherwise      =
          ["-- (" ++ (show $ btNodeKey node) ++ ", " ++ (btAddInfo node) ++ ")"] ++
          (if (not $ btIsEmpty $ btLeft node) then map ("  |" ++) ls else []) ++
          (if (not $ btIsEmpty $ btLeft node) || (not $ btIsEmpty $ btRight node) then ("  `" ++ r) : map ("   " ++) rs else [])
          where
            (r:rs) = treeIndent $ btRight node
            ls     = treeIndent $ btLeft node


