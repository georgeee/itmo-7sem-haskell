{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Test where
import Data.List (length, sort)
import Data.Monoid
import Map as M
import Sets
import Set as S
import LLRB
import DummyMap
import DefaultInstancesLazy
import qualified Data.Map.Lazy as LM
import Prelude hiding (lookup, foldl, foldr)
import qualified Prelude as P
import RandomUtils

insertTest :: (Map k v t, Num v, Enum v) => [k] -> t -> t
insertTest keys m = P.foldr (uncurry M.insert) m $ zip keys [1..]

sumKeys :: (Map k v t, Num k) => t -> k
sumKeys = sum . map fst . M.toList

genRandomMap :: (Map Int Int t) => Int -> t -> IO t
genRandomMap n m = randomIntList n 0 (n `div` 2) >>= return . flip insertTest m

testMap keys = sumKeys . insertTest keys
testRandomMap n m = genRandomMap n m >>= return . sumKeys

-- to see time, enter :set +s

-- *Test> testRandomLLRB 100000
-- 1077665084
-- (19.65 secs, 4700689088 bytes)
-- *Test> testRandomDummy 100000
-- 1081119583
-- (4.00 secs, 1012231448 bytes)
-- *Test> testRandomLazyMap  100000
-- 1079268163
-- (0.57 secs, 211665272 bytes)

testRandomLLRB = flip testRandomMap (M.empty :: LLRBTreeMap Int Int)
testRandomLazyMap = flip testRandomMap (M.empty :: LM.Map Int Int)
testRandomDummy = flip testRandomMap (M.empty :: DummyMap Int Int)

testLLRB = flip testMap (M.empty :: LLRBTreeMap Int Int)
testLazyMap = flip testMap (M.empty :: LM.Map Int Int)
testDummy = flip testMap (M.empty :: DummyMap Int Int)

-- *Test> testLazyMap [1..5000]
-- 12502500
-- (0.01 secs, 21132944 bytes)
-- *Test> testDummy [1..5000]
-- 12502500
-- (17.45 secs, 5952942656 bytes)
-- *Test> testLLRB [1..5000]
-- 12502500
-- (1.11 secs, 264987984 bytes)

testMonoid :: LLRBTreeSet Int
testMonoid = mconcat [(S.fromList [1, 4, 5, 6] :: LLRBTreeSet Int), (S.fromList [11, 14, 15, 16] :: LLRBTreeSet Int), (S.fromList [21, 24, 25, 26] :: LLRBTreeSet Int)]
