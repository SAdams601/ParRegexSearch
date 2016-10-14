{-# LANGUAGE DeriveFunctor #-}
module Test2 where
import qualified Control.Monad as M

data M a = J a
         | N
           deriving (Functor, Eq, Show)

instance Applicative M where
  pure = J
  J f <*> m = fmap f m
  N <*> _ = N
  J _ *> m2 = m2
  N *> _ = N

instance M.Monad M where
  (J x) >>= k = k x
  N >>= _ = N
  return = pure
