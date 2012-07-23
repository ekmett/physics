{-# LANGUAGE RankNTypes #-}
module Physics.Rep
  ( Rep(..)
  , pureRep
  , apRep
  , bindRep
  , distributeRep
  , mapWithKeyRep
  ) where

import Physics.Lens

-- | corepresentable functors, represented directly by their polymorphic lenses
class Functor f => Rep f where
  rep :: ((forall g x. Functor g => (x -> g x) -> f x -> g (f x)) -> a) -> f a

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
