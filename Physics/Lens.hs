-- | A minimal, self-contained lens library
module Physics.Lens
  ( (^.), (%=), (^=)
  , getting
  , iso, lens
  , (+=), (-=), (*=)
  ) where

import Control.Applicative
import Data.Functor.Identity

lens :: Functor f => (a -> c) -> (a -> d -> b) -> (c -> f d) -> a -> f b
lens ac adb cfd a = adb a <$> cfd (ac a)
{-# INLINE lens #-}

iso :: Functor f => (a -> c) -> (d -> b) -> (c -> f d) -> a -> f b
iso f g h a = g <$> h (f a )
{-# INLINE iso #-}

infixl 8 ^.
(^.) :: a -> ((c -> Const c d) -> a -> Const c b) -> c
x ^. l = getConst (l Const x)
{-# INLINE (^.) #-}

infixr 4 %=, ^=, +=
(%=) :: ((c -> Identity d) -> a -> Identity b) -> (c -> d) -> a -> b
l %= f = runIdentity . l (Identity . f)
{-# INLINE (%=) #-}

(^=) :: ((c -> Identity d) -> a -> Identity b) -> d -> a -> b
l ^= v = l %= const v
{-# INLINE (^=) #-}

getting :: (a -> c) -> (c -> Const r d) -> a -> Const r b
getting f g a = Const (getConst (g (f a)))
{-# INLINE getting #-}

(+=), (*=), (-=) :: Num c => ((c -> Identity c) -> a -> Identity a) -> c -> a -> a
l += n = l %= (+ n)
{-# INLINE (+=) #-}
l -= n = l %= (`subtract` n)
{-# INLINE (-=) #-}
l *= n = l %= (* n)
{-# INLINE (*=) #-}
