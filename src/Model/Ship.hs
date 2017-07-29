module Model.Ship 
    (
        Ship (..)
    ) where

import qualified Data.Map as Map

import Linear.V2 (V2(V2))

import qualified Model.Part as Part

data Ship = Ship { id :: Int,
                   name :: String,
                   factionId :: Int,
                   pos :: V2 Double,
                   parts :: Map.Map Int Part.Part,
                   nParts :: Int}
    deriving (Show)
