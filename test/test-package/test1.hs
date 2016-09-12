{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test1 where

newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Show, Eq, Ord, Read, Functor, Foldable)

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)
