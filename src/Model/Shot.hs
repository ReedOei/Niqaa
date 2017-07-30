module Model.Shot
    (
        Shot (..),
        shot_size, shot_speed, shot_damage
    ) where

import Linear.V2 (V2(V2))

shot_size :: Double
shot_size = 5

shot_speed :: Double
shot_speed = 1

shot_damage :: Double
shot_damage = 3

data Shot = Shot { id :: Int,
                   pos :: V2 Double,
                   vel :: V2 Double,
                   size :: Double,
                   shotDamage :: Double,
                   factionId :: Int,
                   launchId :: Int}
    deriving (Show)

