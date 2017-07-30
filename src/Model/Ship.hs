module Model.Ship 
    (
        Ship (..)
    ) where

import qualified Data.Map as Map

import Linear.V2 (V2(V2))

import qualified Model.Part as Part

data Ship = Ship { id :: Int,
                   name :: String,
                   classType :: String, -- e.g. Destroyer, Cruiser, etc.
                   classAbb :: String, -- e.g. DD, CA, etc.
                   color :: (Double, Double, Double), -- The color of this ship and it's shots
                   factionId :: Int,
                   pos :: V2 Double}
    deriving (Show)
