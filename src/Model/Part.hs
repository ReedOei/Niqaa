module Model.Part
    (
        Part (..)
    ) where

import Linear.V2 (V2(V2))

data Part = Part { id :: Int,
                   shipId :: Int,
                   partType :: String,
                   pos :: V2 Double,
                   vel :: V2 Double,
                   health :: Int,
                   size :: Double,
                   timeGoal :: Double,
                   timer :: Double }
    deriving Show
