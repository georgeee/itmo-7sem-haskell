{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (SumList(..), CachedSumList, cList, cSum, cachedSumList, task1, mergeSort) where
import Data.Monoid
import Data.List (splitAt)

task1 = sum . map read . words

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


type Range = (Int, Int)

data Diagnostic t = D Range String

data Warning
data Error
warningT = undefined :: Warning
errorT   = undefined :: Error
overlaps :: Diagnostic t -> Diagnostic t -> Bool
overlaps (D (a, b) _) (D (c, d) _) = let l = max a c
                                         r = min b d
                                      in l <= r
createDiagnostic :: t -> Range -> String -> Diagnostic t
createDiagnostic _ r s = D r s

d1 = createDiagnostic warningT (1, 2) "This is warning"
d2 = D (3, 4) "This is error" :: Diagnostic Error
