module Test where
import Data.List
import Data.Monoid
import Main
import RandomUtils

task1Tests = ["1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030",
              " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 ",
              "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"]
task1MustFail = ["asd", "1-1", "1.2", "--2", "+1", "1+"]

-- x = CachedSumList [1] 0 -- would fail

testSumList :: (Monoid a, Ord a) => ([Int] -> a) -> Int -> Int -> IO Int
testSumList ct retry maxVal = randomIntLists' >>= return . length . filter id . snd
  where randomIntLists' = randomIntLists retry 10 0 maxVal >>= return . run
        run [] = (ct [], [])
        run (x:xs) = let (cs, bs) = run xs
                         cs1 = ct x
                     in ((cs1 `mappend` cs), (cs1 <= cs) : bs)

testSumList1 = testSumList SumList 2000 100
testSumList2 = testSumList cachedSumList 2000 100

testMergeSort retry len maxVal = randomIntLists retry len 0 maxVal >>= sequence_ . (map f)
      where f l = if (mergeSort l) == (sort l) then return () else print l >> fail "Test not passed"

