module Model.Part
    (
        Part (..),
        Stats (..)
    ) where

import Linear.V2 (V2(V2))

import Helm.Color

data Part = Part { id :: Int,
                   name :: String,
                   shipId :: Int,
                   pos :: V2 Double,
                   vel :: V2 Double,
                   health :: Double,
                   color :: Color,
                   size :: Double,
                   factionId :: Int,
                   stats :: Stats}
    deriving (Show, Read)

data Stats = Hull | 
             Gun {
                prec :: Double,
                timerGoal :: Double,
                timer :: Double,
                shotSize :: Double,
                shotDamage :: Double,
                shotSpeed :: Double,
                salvoSize :: Int,
                shotsLeft :: Int,
                salvoTimerGoal :: Double,
                salvoTimer :: Double
             }
    deriving (Show, Read)
