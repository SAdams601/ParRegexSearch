{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test1 where
import Prelude hiding (Applicative)
import qualified Control.Applicative as App

newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Show, Eq, Ord, Read, Functor, Foldable)

instance App.Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)
