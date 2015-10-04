{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module LLRB (LLRBTreeMap) where
import Map
import Data.Monoid
import Prelude hiding (lookup)

class Inversable v where
  inverse :: v -> v

data Color = Red | Black
  deriving Show
data LLRBTreeMap k v = Empty | Node { left :: (LLRBTreeMap k v), right :: (LLRBTreeMap k v), key :: k, value :: v, color :: Color }

instance BSTMap k v (LLRBTreeMap k v) where
  btKey = key
  btValue = value
  btLeft = left
  btRight = right
  btAddInfo = show . color
  btIsEmpty Empty = True
  btIsEmpty _ = False

instance (Show k, Show v) => Show (LLRBTreeMap k v) where
  show = btShow

instance Inversable (LLRBTreeMap k v) where
  inverse Empty = Empty
  inverse node = node { color = inverse $ color node }

instance Inversable Color where
  inverse Red = Black
  inverse Black = Red


isRed (Node {color = Red}) = True
isRed _ = False

isBlack (Node {color = Black}) = True
isBlack _ = False

instance (Ord k) => Map k v (LLRBTreeMap k v) where
  empty = Empty
  insert k v node = (insertImpl k v node) { color = Black }
  delete k node = (deleteImpl k node) { color = Black }
  foldrWithKey = btFoldrWithKey
  lookup' = btLookup'
  isEmpty = btIsEmpty

instance (Ord k) => Monoid (LLRBTreeMap k v) where
  mempty = empty
  mappend = btAppend

colorFlip Empty = Empty
colorFlip n@(Node { left = l@_, right = r@_}) = inverse $ n { left = inverse l, right = inverse r }

rotateLeft a@(Node { left = al@_, right = b@(Node { left = bl@_, right = br@_ }) }) = b { left = a { right = bl, color = Red }, color = color a }

rotateRight b@(Node { left = a@(Node { left = al@_, right = ar@_ }), right = br@_ }) = a { right = b { left = ar, color = Red }, color = color b }

(-&&-) a b x = (a x) && (b x)
infixr 3 -&&-
modIf p f x = if p x then f x else x

rebalance Empty = Empty
rebalance node = (conv3 . conv2 . conv1) node
  where
    conv1 = modIf (isRed . right) rotateLeft
    conv2 = modIf ((isRed . left) -&&- (isRed . left . left)) rotateRight
    conv3 = modIf ((isRed . left) -&&- (isRed . right)) colorFlip


insertImpl k v node | isEmpty node   = Node { left = Empty, right = Empty, key = k, value = v, color = Red }
                    | k == (key node) = node { key = k, value = v }
                    | k < (key node) = rebalance $ node { left = insertImpl k v $ left node }
                    | otherwise      = rebalance $ node { right = insertImpl k v $ right node }

moveRedLeft = modIf (isRed . left . right) (colorFlip . rotateLeft . (\h -> h { right = rotateRight $ right h })) . colorFlip
moveRedRight = modIf (isRed . left . left) (colorFlip . rotateRight) . colorFlip

-- returns pair of tree and deleted node data
deleteMin :: Ord k => LLRBTreeMap k v -> (LLRBTreeMap k v, Maybe (k, v))
deleteMin n@(Node { left = l@(Node {}) }) = (conv3 . conv2 . conv1) n
  where
     conv1 = modIf ((isBlack . left) -&&- (isBlack . left . left)) moveRedLeft
     conv2 n@(Node { left = l@(Node {}) }) = let (l', deletedData) = deleteMin l
                                             in ( n { left = l' }, deletedData)
     conv3 (n, deletedData) = (rebalance n, deletedData)
deleteMin (Node { left = Empty, key=k@_, value=v@_ }) = (Empty, Just (k, v))
deleteMin n = (n, Nothing)

deleteImpl :: Ord k => k -> LLRBTreeMap k v -> LLRBTreeMap k v
deleteImpl k = rebalance . deleteImpl' k

deleteImpl' :: Ord k => k -> LLRBTreeMap k v -> LLRBTreeMap k v
deleteImpl' k n | isEmpty n = n
                | k < (key n) = delegateLeft k $ conv1 n
                | otherwise = deleteImpl'' k $ conv2 n
  where
    conv1 = modIf ((isBlack . left) -&&- (isBlack . left . left)) moveRedLeft
    conv2 = modIf (isRed . left) rotateRight

delegateLeft k n = n { left = deleteImpl k $ left n }

deleteImpl'' :: Ord k => k -> LLRBTreeMap k v -> LLRBTreeMap k v
deleteImpl'' k n | k == (key n) && (isEmpty $ right n) = Empty
                 | otherwise                         = deleteImpl''' k $ conv1 n
  where
    conv1 = modIf ((isBlack . right) -&&- (isBlack . left . right)) moveRedRight

deleteImpl''' :: Ord k => k -> LLRBTreeMap k v -> LLRBTreeMap k v
deleteImpl''' k n | k == (key n) = let (r', Just (k, v)) = deleteMin $ right n
                                  in n { right = r', key = k, value = v }
                  | otherwise   = n { right = deleteImpl k $ right n }

