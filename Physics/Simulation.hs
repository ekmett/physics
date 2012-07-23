module Physics.Simulation
  ( Simulation(..)
  ) where

class Simulation t where
  init   :: Double -> Double -> Double -> t
  update :: Double -> t -> t

{-

class Simulation t where
  init :: IO t
  update :: Double -> t -> IO ()
  cleanup :: IO ()
  version :: IO String
  time :: IO Double
  lastTimeStep :: IO Double

-}
