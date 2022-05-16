{-# LANGUAGE FlexibleContexts #-}

module FunctionalStreams () where

import Control.Applicative
import Control.Arrow
import Data.Foldable (Foldable (fold))

--import Stream.Internal

-- Defined in Stream.Internal:
--     data Stream a = a :> Stream a
--     infixr :>

data Stream a = a :> Stream a deriving (Show)

-- | Get the first element of a stream.
headS :: Stream a -> a
headS (x :> xs) = x

-- | Drop the first element of a stream.
tailS :: Stream a -> Stream a
tailS (x :> xs) = xs

-- {{{ Stream constructors

-- | Construct a stream by repeating a value.
repeatS :: a -> Stream a
repeatS x = x :> repeatS x

-- | Construct a stream by repeatedly applying a function.
iterateS :: (a -> a) -> a -> Stream a
iterateS f x = x :> iterateS f (f x)

-- | Construct a stream by repeating a list forever.
cycleS :: [a] -> Stream a
cycleS (x : xs) = x :> cycleS (xs ++ [x])
cycleS [] = error "empty"

cycleS' xs = foldr (:>) (cycleS' xs) xs

-- | Construct a stream by counting numbers starting from a given one.
fromS :: Num a => a -> Stream a
fromS n = n :> fromS (n + 1)

-- | Same as 'fromS', but count with a given step width.
fromStepS :: Num a => a -> a -> Stream a
fromStepS x s = x :> fromStepS (x + s) s

-- }}}

-- | Fold a stream from the left.
foldrS :: (a -> b -> b) -> Stream a -> b
foldrS f (x :> xs) = f x (foldrS f xs)

-- | Filter a stream with a predicate.
filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (x :> xs) =
  if p x
    then x :> filterS p xs
    else filterS p xs

-- | Take a given amount of elements from a stream.
takeS :: Int -> Stream a -> [a]
takeS i (x :> xs)
  | i <= 0 = []
  | otherwise = x : takeS (i - 1) xs

-- | Drop a given amount of elements from a stream.
dropS :: Int -> Stream a -> Stream a
dropS i full@(x :> xs)
  | i <= 0 = full
  | otherwise = dropS (i - 1) xs

-- | Do take and drop simultaneous.
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS i s = (,) (takeS i s) (dropS i s)

splitAtS' i (x :> xs)
  | i <= 0 = ([], x :> xs)
  | otherwise = (x : fst n, snd n)
  where
    n = splitAtS' (i - 1) xs

-- | Combine two streams with a function.
zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f (x :> xs) (y :> ys) = f x y :> zipWithS f xs ys

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS = zipWithS (,)

instance Functor Stream where
  fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
  -- pure :: a -> Stream a
  pure = repeatS
  (<*>) = zipWithS ($)
--(f :> fs) <*> (x :> xs) = f x :> (fs <*> xs)

s :: Stream Integer
s = repeatS 1

-- | The stream of fibonacci numbers.
fibS :: Stream Integer
fibS = 0 :> (1 :> zipWithS (+) fibS (tailS fibS))

-- | The stream of prime numbers.
primeS :: Stream Integer
primeS = filterS isPrim $ fromS 2
  where
    isPrim n = all (\x -> n `mod` x /= 0) [2 .. (n - 1)]