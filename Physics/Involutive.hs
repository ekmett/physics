module Physics.Involutive
  ( Involutive(..)
  ) where

import Data.Complex hiding (conjugate)

class Involutive a where
  conjugate :: a -> a
  conjugate = id

instance Involutive Double
instance Involutive Float
instance (Involutive a, RealFloat a) => Involutive (Complex a) where
  {-# SPECIALIZE instance Involutive (Complex Float) #-}
  {-# SPECIALIZE instance Involutive (Complex Double) #-}
  conjugate (a :+ b) = conjugate a :+ negate b
