module Physics.Simulation
  ( Simulation(..)
  ) where

import Physics.V3

-- simulations are defined in terms of meters, kilograms and seconds

class Simulation t where
  init   :: Fractional a => V3 a -> t a     -- initialize with gravity vector
  update :: Fractional a => a -> t a -> t a

{-

class Simulation t where
  init :: IO t
  update :: Double -> t -> IO ()
  cleanup :: IO ()
  version :: IO String
  time :: IO Double
  lastTimeStep :: IO Double

-}
