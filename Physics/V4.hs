{-# LANGUAGE DeriveDataTypeable #-}
module Physics.V4
  ( V4(..)
  , vector, point
  , D2(..)
  , D3(..)
  , D4(..)
  ) where

import Control.Applicative
import Data.Data
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Physics.V2
import Physics.V3
import Physics.Metric

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
  V4 a b c d >>= f = V4 a' b' c' d' where
    V4 a' _ _ _ = f a
    V4 _ b' _ _ = f b
    V4 _ _ c' _ = f c
    V4 _ _ _ d' = f d

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

class D3 t => D4 t where
  w :: Functor f => (a -> f a) -> t a -> f (t a)
  xyzw :: Functor f => (V4 a -> f (V4 a)) -> t a -> f (t a)

instance D2 V4 where
  x f (V4 a b c d) = (\a' -> V4 a' b c d) <$> f a
  y f (V4 a b c d) = (\b' -> V4 a b' c d) <$> f b
  xy f (V4 a b c d) = (\(V2 a' b') -> V4 a' b' c d) <$> f (V2 a b)

instance D3 V4 where
  z f (V4 a b c d) = (\c' -> V4 a b c' d) <$> f c
  xyz f (V4 a b c d) = (\(V3 a' b' c') -> V4 a' b' c' d) <$> f (V3 a b c)

instance D4 V4 where
  w f (V4 a b c d) = V4 a b c <$> f d
  xyzw = id

vector :: Num a => V3 a -> V4 a
vector (V3 a b c) = V4 a b c 0

point :: Num a => V3 a -> V4 a
point (V3 a b c) = V4 a b c 1
