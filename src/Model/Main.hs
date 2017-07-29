module Model.Main
    (
        Action (..),
        Model (..),
        Faction (..),
        Rect (..),
        Direction (..)
    ) where

import qualified Data.Map as Map

import Linear.V2 (V2(V2))

import qualified Model.Shot as Shot
import qualified Model.Ship as Ship

data Direction = U | D | L | R

data Action = LClick (V2 Double) | RClick (V2 Double) | None | Step Double
    deriving Show
data Model = Model { currentShip :: Int, -- The id of the current ship
                     ships :: Map.Map Int Ship.Ship,
                     shots :: Map.Map Int Shot.Shot,
                     nShips :: Int, 
                     nShots :: Int}
    deriving Show

data Faction = Faction { factionId :: Int,
                         factioNName :: String}
    deriving Show

data Rect = Rect Double Double Double Double