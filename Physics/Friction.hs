module Physics.Friction
  ( Friction(..)
  ) where

import Data.Default

-- Using a Coulomb model of friction
data Friction = Friction
  { staticFriction  :: {-# UNPACK #-} !Double -- ^ the amount of friction when the object starts to move
  , kineticFriction :: {-# UNPACK #-} !Double -- ^ the amount of friction when the object is already in motion
  , restitution     :: {-# UNPACK #-} !Double -- ^ the height an object will bounce when dropped onto the material
  }

instance Default Friction where
  def = Friction 0 0 0.5
