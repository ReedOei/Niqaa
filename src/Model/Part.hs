module Model.Part
    (
        Part (..),
        Stats (..)
    ) where

import Linear.V2 (V2(V2))

data Part = Part { id :: Int,
                   shipId :: Int,
                   pos :: V2 Double,
                   vel :: V2 Double,
                   health :: Double,
                   size :: Double,
                   factionId :: Int,
                   stats :: Stats}
    deriving Show

data Stats = Hull | 
             Gun Double Double Double -- Precision, timer goal, timer
    deriving Show
