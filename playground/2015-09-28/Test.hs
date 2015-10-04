{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Test where

class Set a t | t -> a where
  find :: a -> t -> Bool
  add :: a -> t -> t

data MySet a = Empty | Node { key :: a, left :: MySet a, right :: MySet a }

instance (Ord a) => Set a (MySet a) where
  find _ Empty = False
  find _ (Node _ _ _) = True
  add x Empty = Node x Empty Empty

instance Set Int (MySet Int) where
  add = undefined
  find = undefined

