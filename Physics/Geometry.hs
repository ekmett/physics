module Physics.Geometry
  ( Geometry(..)
  ) where

import Physics.V3

class Geometry t where
  position  :: t a -> V3 a
  mass      :: t a -> a
  inertia   :: t a -> V3 a -- the diagonal of the Moment of inertia tensor
