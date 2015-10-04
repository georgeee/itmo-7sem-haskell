{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module CW1 where
import Data.Monoid
import Data.Function
import Data.List
import Data.Maybe

task1 :: (Integral a) => [a] -> [Integer] -- we assume list can be of length more than 2^31
task1 = map snd . filter ((==0) . (`mod` 2) . fst) . (flip zip [0..])

--Functional dependincies could be avoided, but then we would have to explicitly declare both argument's params
-- E.g. following piece in CLI would produce a can't deduce type warning:
-- push 2 []
-- you would have to write smth like push (2::Int) ([]::[Int])
-- with FunctionalDependencies however everything works fine out-of-box

class Stack a t | t -> a where
  pop :: t -> (Maybe a, t)
  push :: a -> t -> t
  emptyStack :: t

class Queue a t | t -> a where
  remove :: t -> (Maybe a, t)
  add :: a -> t -> t
  emptyQueue :: t
  toList :: t -> [a]
  toList = map (fromJust.fst) . takeWhile (isJust . fst) . iterate (\(x, y) -> let (a, b) = remove y in (a, b)) . remove

type MyStack a = [a]

instance Stack a [a] where
  pop (a:as) = (Just a, as)
  pop ms = (Nothing, ms)
  push = (:)
  emptyStack = []

data MyQueue a st = MyQueue { st1 :: st, st2 :: st }
  deriving Show

moveAllReverse :: (Stack a s1, Stack a s2) => s1 -> s2 -> (s1, s2)
moveAllReverse s1 s2 = let (s1', els) = popAll s1
                       in (s1', foldl (flip push) s2 (reverse els))

popAll :: (Stack a s1) => s1 -> (s1, [a])
popAll s1 = popAll' s1 []
  where popAll' s els = case pop s of
                          (Nothing, _) -> (s, els)
                          (Just x, s') -> popAll' s' (x:els)

instance (Stack a t) => Queue a (MyQueue a t) where
  remove mq@(MyQueue s1 s2) = case pop s2 of
                     (Nothing, _) -> case pop s1 of
                                       (Nothing, _) -> (Nothing, mq)
                                       (_, _) -> remove . uncurry MyQueue $ moveAllReverse s1 s2
                     (Just a, s2') -> (Just a, MyQueue s1 s2')
  add el q = q { st1 = push el $ st1 q }
  emptyQueue = MyQueue emptyStack emptyStack


type MyArrayQueue a = MyQueue a [a]

x1 = foldl (flip add) (emptyQueue :: MyArrayQueue Int) [1, 2, 3, 4]


instance (Stack a t) => Monoid (MyQueue a t) where
  mempty = emptyQueue
  mappend a b = case remove b of
                  (Nothing, _) -> a
                  (Just x, b') -> mappend (add x a) b'

