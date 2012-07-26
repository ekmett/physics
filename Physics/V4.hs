{-# LANGUAGE DeriveDataTypeable #-}
module Physics.V4
  ( V4(..)
  , vector, point
  , R2(..)
  , R3(..)
  , R4(..)
  ) where

import Control.Applicative
import Control.Lens.Rep
import Data.Data
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Physics.Epsilon
import Physics.Metric
import Physics.V2
import Physics.V3

data V4 a = V4 a a a a deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Functor V4 where
  fmap f (V4 a b c d) = V4 (f a) (f b) (f c) (f d)

instance Foldable V4 where
  foldMap f (V4 a b c d) = f a `mappend` f b `mappend` f c `mappend` f d

instance Traversable V4 where
  traverse f (V4 a b c d) = V4 <$> f a <*> f b <*> f c <*> f d

instance Applicative V4 where
  pure a = V4 a a a a
  V4 a b c d <*> V4 e f g h = V4 (a e) (b f) (c g) (d h)

instance Monad V4 where
  return a = V4 a a a a
  (>>=) = bindRep

instance Num a => Num (V4 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (V4 a) where
  recip = fmap recip
  (/) = liftA2 (/)
  fromRational = pure . fromRational

instance Metric V4 where
  dot (V4 a b c d) (V4 e f g h) = a * e + b * f + c * g + d * h

class R3 t => R4 t where
  w :: Functor f => (a -> f a) -> t a -> f (t a)
  xyzw :: Functor f => (V4 a -> f (V4 a)) -> t a -> f (t a)

instance R2 V4 where
  x f (V4 a b c d) = (\a' -> V4 a' b c d) <$> f a
  y f (V4 a b c d) = (\b' -> V4 a b' c d) <$> f b
  xy f (V4 a b c d) = (\(V2 a' b') -> V4 a' b' c d) <$> f (V2 a b)

instance R3 V4 where
  z f (V4 a b c d) = (\c' -> V4 a b c' d) <$> f c
  xyz f (V4 a b c d) = (\(V3 a' b' c') -> V4 a' b' c' d) <$> f (V3 a b c)

instance R4 V4 where
  w f (V4 a b c d) = V4 a b c <$> f d
  xyzw = id

instance Representable V4 where
  rep f = V4 (f x) (f y) (f z) (f w)

vector :: Num a => V3 a -> V4 a
vector (V3 a b c) = V4 a b c 0

point :: Num a => V3 a -> V4 a
point (V3 a b c) = V4 a b c 1

instance Epsilon a => Epsilon (V4 a) where
  nearZero = nearZero . quadrance
