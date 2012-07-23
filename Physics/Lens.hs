module Physics.Lens
  ( (^.), (%=), (^=)
  , getting
  ) where

import Control.Applicative
import Data.Functor.Identity

infixl 8 ^.
(^.) :: a -> ((c -> Const c d) -> a -> Const c b) -> c
x ^. l = getConst (l Const x)

infixr 4 %=
(%=) :: ((c -> Identity d) -> a -> Identity b) -> (c -> d) -> a -> b
l %= f = runIdentity . l (Identity . f)

infixr 4 ^=
(^=) :: ((c -> Identity d) -> a -> Identity b) -> d -> a -> b
l ^= v = l %= const v

getting :: (a -> c) -> (c -> Const r d) -> a -> Const r b
getting f g a = Const (getConst (g (f a)))
