{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Map (Map(..), BSTMap(..), btShow, btFoldrWithKey, btMax, btMin, btLookup', btAppend) where
import Prelude hiding (lookup, foldl, foldr)
import qualified Prelude as P

class Map k v t | t -> k, t -> v where
  empty :: t
  isEmpty :: t -> Bool
  insert :: k -> v -> t -> t
  delete :: k -> t -> t
  delete = update (const Nothing)
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

  foldlWithKey :: (b -> k -> v -> b) -> b -> t -> b
  foldlWithKey f h = P.foldl (\h -> uncurry $ f h) h . toList

  foldrWithKey :: (k -> v -> b -> b) -> b -> t -> b

  foldr :: (v -> b -> b) -> b -> t -> b
  foldr f = foldrWithKey $ const f

  foldl :: (b -> v -> b) -> b -> t -> b
  foldl f = foldlWithKey $ \h -> const $ f h

  toList :: t -> [(k, v)]
  toList = foldrWithKey (curry (:)) []

  lookup' :: k -> t -> (Maybe (k, v), Maybe (k, v), Maybe (k, v))

  next :: k -> t -> Maybe (k, v)
  next k n = let (_, _, next) = lookup' k n in next

  prev :: k -> t -> Maybe (k, v)
  prev k n = let (prev, _, _) = lookup' k n in prev

  lookup :: k -> t -> Maybe v
  lookup k n = let (_, cur, _) = lookup' k n in maybe Nothing (Just . snd) cur

class BSTMap k v n | n -> k, n -> v where
  btKey :: n -> k
  btLeft :: n -> n
  btRight :: n -> n
  btValue :: n -> v
  btAddInfo :: n -> String
  btIsEmpty :: n -> Bool
  btPair :: n -> (k, v)
  btPair n = (btKey n, btValue n)


data NeedLookup = G | L | GL | N

btAppend :: (BSTMap k v t, Map k v t) => t -> t -> t
btAppend = foldrWithKey insert

btMin :: (Ord k, BSTMap k v n) => n -> Maybe (k, v)
btMin n | btIsEmpty n = Nothing
        | btIsEmpty (btLeft n) = Just $ btPair n
        | otherwise = btMin $ btLeft n

btMax :: (Ord k, BSTMap k v n) => n -> Maybe (k, v)
btMax n | btIsEmpty n = Nothing
        | btIsEmpty (btRight n) = Just $ btPair n
        | otherwise = btMax $ btRight n

btLookup' :: (Ord k, BSTMap k v n) => k -> n -> (Maybe (k, v), Maybe (k, v), Maybe (k, v))
btLookup' k = btl' Nothing Nothing
  where
    btl' pr ne n | btIsEmpty n = (pr, Nothing, ne)
                 | k == (btKey n) = let cur = Just $ btPair n
                                        prev = if btIsEmpty $ btLeft n then pr else btMax $ btLeft n
                                        next = if btIsEmpty $ btRight n then ne else btMin $ btRight n
                                   in (prev, cur, next)
                 | k < (btKey n) = btl' pr (Just $ btPair n) $ btLeft n
                 | otherwise = btl' (Just $ btPair n) ne $ btRight n

btFoldrWithKey :: (BSTMap k v n) => (k -> v -> b -> b) -> b -> n -> b
btFoldrWithKey f b n |btIsEmpty n  = b
                     |otherwise = let b' = btFoldrWithKey f b $ btRight n
                                      b'' = f (btKey n) (btValue n) b'
                                      b''' = btFoldrWithKey f b'' $ btLeft n
                                  in b'''

btShow :: (Show k, Show v, BSTMap k v n) => n -> String
btShow = foldl1 (\xs x -> (xs ++ "\n" ++ x)) . treeIndent
  where treeIndent node |btIsEmpty node = ["-- /-"]
                        |otherwise      =
          ["-- " ++ (show $ (btKey node, btValue node, btAddInfo node))] ++
          (if (not $ btIsEmpty $ btLeft node) then map ("  |" ++) ls else []) ++
          (if (not $ btIsEmpty $ btLeft node) || (not $ btIsEmpty $ btRight node) then ("  `" ++ r) : map ("   " ++) rs else [])
          where
            (r:rs) = treeIndent $ btRight node
            ls     = treeIndent $ btLeft node


