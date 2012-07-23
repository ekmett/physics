module Physics.Geometry.Sphere
  ( Sphere(..)
  ) where

import Physics.Geometry
import Physics.V3

data Sphere a = Sphere
  { sphereRadius   :: a
  , sphereMass     :: a
  , spherePosition :: V3 a
  , sphereInertia  :: V3 a
  }

instance Geometry Sphere where
  position = spherePosition
  mass     = sphereMass
  inertia  = sphereInertia
