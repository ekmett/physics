module Physics.AABB
  ( AABB(..)
  , valid
  , diameter
  , union
  , vertices
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Data.Semigroup
import Data.Foldable as Foldable
import Data.Traversable
import Physics.Lens
import Physics.Metric

data AABB f a = AABB (f a) (f a) deriving (Eq,Ord,Show,Read)

instance Functor f => Functor (AABB f) where
  fmap f (AABB lo hi) = AABB (fmap f lo) (fmap f hi)

instance Applicative f => Applicative (AABB f) where
  pure a = AABB as as where as = pure a
  AABB l h <*> AABB l' h' = AABB (l <*> l') (h <*> h')

instance Monad f => Monad (AABB f) where
  return a = AABB as as where as = return a
  AABB l h >>= f = AABB (l >>= (^.lo) . f) (h >>= (^.hi) . f)

instance MonadTrans AABB where
  lift m = AABB m m

instance Foldable f => Foldable (AABB f) where
  foldMap f (AABB l h) = foldMap f l `mappend` foldMap f h

instance Traversable f => Traversable (AABB f) where
  traverse f (AABB l h) = AABB <$> traverse f l <*> traverse f h

valid :: (Applicative f, Foldable f, Ord a) => AABB f a -> Bool
valid (AABB l h) = Foldable.and $ liftA2 (<=) l h

diameter :: (Metric f, Floating a) => AABB f a -> a
diameter (AABB l h) = distance h l

union :: (Applicative f, Ord a) => AABB f a -> AABB f a -> AABB f a
union (AABB l h) (AABB l' h') = AABB (liftA2 min l l') (liftA2 max h h')

vertices :: (Traversable f, Applicative f) => AABB f a -> [f a]
vertices (AABB l h) = sequenceA $ liftA2 (\a b -> [a,b]) l h

instance (Applicative f, Ord a) => Semigroup (AABB f a) where
  (<>) = union

lo, hi :: Functor g => (f a -> g (f a)) -> AABB f a -> g (AABB f a)
lo f (AABB l h) = (`AABB` h) <$> f l
hi f (AABB l h) = AABB l <$> f h
