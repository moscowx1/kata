{-# LANGUAGE NoImplicitPrelude #-}

module FiveFundamentalMonads where

import Data.Monoid
import Prelude hiding (Identity, Maybe (..), Monad, Reader, State, Writer)

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

newtype Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

newtype State s a = State {runState :: s -> (a, s)}

newtype Reader s a = Reader {runReader :: s -> a}

newtype Writer w a = Writer {runWriter :: (w, a)}

newtype K s = K {m :: s}

instance Monad K where
  return x = K {m = x}
  (K x) >>= f = f x

instance Monad Identity where
  return = Identity
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return s = State $ (,) s

  f >>= g = State $ \x ->
    let (a1, s1) = runState f x
     in runState (g a1) s1

instance Monad (Reader s) where
  return x = Reader $ const x
  g >>= f = Reader $ \r -> runReader (f (runReader g r)) r

instance Monoid w => Monad (Writer w) where
  return x = Writer (mempty, x)
  g >>= f =
    let (w1, a1) = runWriter g
        (w2, a2) = runWriter (f a1)
        ws = mconcat [w1, w2]
     in Writer (ws, a2)
