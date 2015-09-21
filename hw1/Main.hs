{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (SumList(..), CachedSumList, cList, cSum, cachedSumList, task1, mergeSort, insert, find, delete, fromList, TreeMap) where
import Data.Monoid
import Data.List (splitAt)

task1 = (foldl (+) 0) . map read . words

newtype SumList a = SumList [a]
  deriving (Show, Monoid)
list (SumList a) = a
-- instance (Num a, Ord a) => Eq (SumList a) where
--   (==) (SumList a) (SumList b) = (foldr (+) 0 a) == (foldr (+) 0 b)
-- instance (Num a, Ord a) => Ord (SumList a) where
--   (<=) (SumList a) (SumList b) = (foldr (+) 0 a) <= (foldr (+) 0 b)

sumL :: (Num a) => SumList a -> a
sumL = sum . list

instance (Ord a, Num a) => Eq (SumList a) where
      l1 == l2 = sumL l1 == sumL l2
instance (Ord a, Num a) => Ord (SumList a) where
      l1 <= l2 = sumL l1 <= sumL l2

data CachedSumList a = CachedSumList { cList :: [a], cSum :: a }

cachedSumList :: (Num a) => [a] -> CachedSumList a
cachedSumList xs = CachedSumList xs (sum xs)

instance (Num a) => Monoid (CachedSumList a) where
  mempty = cachedSumList []
  mappend (CachedSumList xs xsum) (CachedSumList ys ysum) = CachedSumList (xs `mappend` ys) (xsum + ysum)

instance (Show a) => Show (CachedSumList a) where
  show (CachedSumList a b) = (show a) ++ " {" ++ (show b) ++ "}"

instance (Ord a, Num a) => Eq (CachedSumList a) where
      l1 == l2 = (cSum l1) == (cSum l2)
instance (Ord a, Num a) => Ord (CachedSumList a) where
      l1 <= l2 = (cSum l1) <= (cSum l2)


splitToHalves xs = splitAt ((length xs) `div` 2) xs

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = let (l, r) = splitToHalves xs
                   l' = mergeSort l
                   r' = mergeSort r
               in merge' l' r'
  where merge' as [] = as
        merge' [] bs = bs
        merge' (a:as) (b:bs) = if a <= b then a : (merge' as (b:bs)) else b : (merge' (a:as) bs)

data TreeMap k v = Empty | Node k v (TreeMap k v) (TreeMap k v)

insert   :: (Ord k) => k -> v -> (TreeMap k v) -> (TreeMap k v)
find     :: (Ord k) => k -> (TreeMap k v) -> Maybe v
fromList :: (Ord k) => [(k, v)] -> (TreeMap k v)
delete   :: (Ord k) => k -> (TreeMap k v) -> TreeMap k v

insert k v Empty = Node k v Empty Empty
insert k v (Node k' v' l r) | k == k' = Node k v l r
                            | k < k' = Node k' v' (ins l) r
                            | k > k' = Node k' v' l (ins r)
      where ins = insert k v

find _ Empty = Nothing
find k (Node k' v' l r) | k == k' = Just v'
                        | k < k' = find k l
                        | k > k' = find k r

fromList = foldl (flip $ uncurry insert) Empty

delete _ Empty = Empty
delete k (Node k' v' l r) | k == k' = merge l r
                          | k < k' = delete k l
                          | k > k' = delete k r

merge :: (Ord k) => (TreeMap k v) -> (TreeMap k v) -> TreeMap k v
merge Empty n = n
merge n Empty = n
merge (Node k v l Empty) r = Node k v l r
merge l r = Node k v l' r
  where (l', Node k v Empty Empty) = pickupR l
        pickupR (Node pk pv pl n@(Node nk nv nl Empty)) = (Node pk pv pl nl, Node nk nv Empty Empty)
        pickupR p@(Node pk pv pl n@(Node _ _ _ nr)) = let (n', res) = pickupR n
                                                       in (Node pk pv pl n', res)



-- красивый вывод дерева
instance (Show k, Show v) => Show (TreeMap k v) where
  show = foldl1 (\xs x -> (xs ++ "\n" ++ x)) . treeIndent
    where treeIndent Empty           = ["-- /-"]
          treeIndent (Node k v lb rb) =
            ["-- (" ++ (show k) ++ ", " ++ (show v) ++ ")"] ++
            map ("  |" ++) ls ++
            ("  `" ++ r) : map ("   " ++) rs
            where
              (r:rs) = treeIndent rb
              ls     = treeIndent lb
