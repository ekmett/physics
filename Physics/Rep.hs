{-# LANGUAGE RankNTypes #-}
module Physics.Rep
  ( Rep(..)
  , fmapRep
  , pureRep
  , apRep
  , bindRep
  , distributeRep
  , mapWithKeyRep
  , Key(..)
  , keys
  ) where

import Physics.Lens

-- | corepresentable functors, represented by their polymorphic lenses
class Functor f => Rep f where
  rep :: ((forall g x. Functor g => (x -> g x) -> f x -> g (f x)) -> a) -> f a

fmapRep :: Rep f => (a -> b) -> f a -> f b
fmapRep f m = rep $ \i -> f (m ^. i)

pureRep :: Rep f => a -> f a
pureRep = rep . const

apRep :: Rep f => f (a -> b) -> f a -> f b
apRep mf ma = rep $ \i -> mf^.i $ ma^.i

bindRep :: Rep f => f a -> (a -> f b) -> f b
bindRep m f = rep $ \i -> f (m^.i) ^.i

distributeRep :: (Rep f, Functor w) => w (f a) -> f (w a)
distributeRep wf = rep $ \i -> fmap (^.i) wf

mapWithKeyRep :: Rep f => ((forall g x. Functor g => (x -> g x) -> f x -> g (f x)) -> a -> b) -> f a -> f b
mapWithKeyRep f m = rep $ \i -> f i (m ^. i)

newtype Key f = Key { turn :: forall g x. Functor g => (x -> g x) -> f x -> g (f x) }

keys :: Rep f => f (Key f)
keys = rep Key
