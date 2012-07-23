{-# LANGUAGE DeriveDataTypeable #-}
-- {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Physics.V2
  ( V2(..)
  , D2(..)
  , perp
  ) where

import Data.Data
import Data.Distributive
import Control.Applicative
import Physics.Metric
import Physics.Lens

data V2 a = V2 a a deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Functor V2 where
  fmap f (V2 a b) = V2 (f a) (f b)

instance Applicative V2 where
  pure a = V2 a a
  V2 a b <*> V2 d e = V2 (a d) (b e)

instance Monad V2 where
  return a = V2 a a
  V2 a b >>= f = V2 c d where
    V2 c _ = f a
    V2 _ d = f b

instance Num a => Num (V2 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (V2 a) where
  recip = fmap recip
  (/) = liftA2 (/)
  fromRational = pure . fromRational

instance Metric V2 where
  dot (V2 a b) (V2 c d) = a * c + b * d

class D2 t where
  x :: Functor f => (a -> f a) -> t a -> f (t a)
  x = xy . x

  y :: Functor f => (a -> f a) -> t a -> f (t a)
  y = xy . y

  xy :: Functor f => (V2 a -> f (V2 a)) -> t a -> f (t a)

instance D2 V2 where
  x f (V2 a b) = (`V2` b) <$> f a
  y f (V2 a b) = (V2 a) <$> f b
  xy = id

instance Distributive V2 where
  distribute f = V2 (fmap (^.x) f) (fmap (^.y) f)

-- the counter-clockwise perpendicular vector
perp :: Num a => V2 a -> V2 a
perp (V2 a b) = V2 (negate b) a
