module Model.Shot
    (
        Shot (..)
    ) where

import Linear.V2 (V2(V2))

import Helm.Color

data Shot = Shot { id :: Int,
                   pos :: V2 Double,
                   vel :: V2 Double,
                   size :: Double,
                   shotDamage :: Double,
                   shotColor :: Color,
                   factionId :: Int,
                   launchId :: Int}
    deriving (Show)

