module RandomUtils where
import System.Random
import Control.Monad

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = fmap (take n . randomRs (from, to) . mkStdGen) randomIO

randomIntLists retry n from to = sequence (take retry . repeat $ randomIntList n from to)
