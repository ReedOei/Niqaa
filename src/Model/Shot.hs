module Model.Shot
    (
        Shot (..)
    ) where

import Linear.V2 (V2(V2))

data Shot = Shot { id :: Int,
                   pos :: V2 Double,
                   vel :: V2 Double,
                   size :: Double,
                   launchId :: Int}
    deriving (Show)

