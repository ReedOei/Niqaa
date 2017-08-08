module Model.Main
    (
        Action (..),
        Model (..),
        Faction (..),
        Shape (..),
        Direction (..),
        BuildItem, BuildPattern,
        world_width, world_height,
        gameFPS
    ) where

import qualified Data.Map as Map

import Linear.V2 (V2(V2))

import System.Random

import qualified Model.Part as Part
import qualified Model.Shot as Shot
import qualified Model.Ship as Ship
import Model.Action

import GUI.Main

world_width :: Double
world_width = 1200

world_height :: Double
world_height = 700

data Model = Model { currentShip :: Int, -- The id of the current ship
                     ships :: Map.Map Int Ship.Ship,
                     shots :: Map.Map Int Shot.Shot,
                     parts :: Map.Map Int Part.Part,
                     nShips :: Int,
                     nShots :: Int,
                     nParts :: Int,
                     worldSize :: V2 Double,
                     gen :: StdGen,
                     shipPatterns :: [BuildPattern],
                     zoomAmount :: Double,
                     guiManager :: GUIManager}
    deriving Show

data Faction = Faction { factionId :: Int,
                         factioNName :: String}
    deriving Show

gameFPS :: Int
gameFPS = 60
