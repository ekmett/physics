module Physics.Material
  ( Material(..)
  ) where

import Physics.Friction
import Data.Default

class Material a where
  materialFriction       :: a -> Friction
  materialFriction _ = def

  interactiveFriction    :: a -> a -> Friction
  interactiveFriction a _ = materialFriction a

  -- anisotropicFriction :: a -> Maybe (V3,V3,V3)
