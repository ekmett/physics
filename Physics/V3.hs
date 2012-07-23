{-# LANGUAGE DeriveDataTypeable #-}
module Physics.V3
  ( V3(..)
  , cross, triple
  , D2(..)
  , D3(..)
  ) where

import Control.Applicative
import Data.Data
import Data.Distributive
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Physics.Metric
import Physics.Lens
import Physics.V2
import Physics.Rep

data V3 a = V3 a a a deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Functor V3 where
  fmap f (V3 a b c) = V3 (f a) (f b) (f c)

instance Foldable V3 where
  foldMap f (V3 a b c) = f a `mappend` f b `mappend` f c

instance Traversable V3 where
  traverse f (V3 a b c) = V3 <$> f a <*> f b <*> f c

instance Applicative V3 where
  pure a = V3 a a a
  V3 a b c <*> V3 d e f = V3 (a d) (b e) (c f)

instance Monad V3 where
  return a = V3 a a a
  (>>=) = bindRep

instance Num a => Num (V3 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (V3 a) where
  recip = fmap recip
  (/) = liftA2 (/)
  fromRational = pure . fromRational

instance Metric V3 where
  dot (V3 a b c) (V3 d e f) = a * d + b * e + c * f

instance Distributive V3 where
  distribute f = V3 (fmap (^.x) f) (fmap (^.y) f) (fmap (^.z) f)

class D2 t => D3 t where
  z :: Functor f => (a -> f a) -> t a -> f (t a)
  xyz :: Functor f => (V3 a -> f (V3 a)) -> t a -> f (t a)

instance D2 V3 where
  x f (V3 a b c) = (\a' -> V3 a' b c) <$> f a
  y f (V3 a b c) = (\b' -> V3 a b' c) <$> f b
  xy f (V3 a b c) = (\(V2 a' b') -> V3 a' b' c) <$> f (V2 a b)

instance D3 V3 where
  z f (V3 a b c) = V3 a b <$> f c
  xyz = id

instance Rep V3 where
  rep f = V3 (f x) (f y) (f z)

-- | cross product
cross :: Num a => V3 a -> V3 a -> V3 a
cross (V3 a b c) (V3 d e f) = V3 (b*f-c*e) (c*d-a*f) (a*e-b*d)

-- | scalar triple product
triple :: Num a => V3 a -> V3 a -> V3 a -> a
triple a b c = dot a (cross b c)
