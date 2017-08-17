module Model.Shot
    (
        Shot (..),
        Stats (..)
    ) where

import Linear.V2 (V2(V2))

import Helm.Color

data Shot = Shot { id :: Int,
                   pos :: V2 Double,
                   vel :: V2 Double,
                   size :: V2 Double,
                   shotDamage :: Double,
                   shotColor :: Color,
                   stats :: Stats,
                   factionId :: Int,
                   launchId :: Int}
    deriving (Show)

data Stats = Pulse |
             Missile { missileFuel :: Double, -- Fuel is consumed at a rate of 1 unit per second.
                       missileAcceleration :: Double } |
             Laser { laserLength :: Double }
    deriving (Show, Read, Eq)

