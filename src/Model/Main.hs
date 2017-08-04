module Model.Main
    (
        Action (..),
        Model (..),
        Faction (..),
        Rect (..),
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

world_width :: Double
world_width = 1200

world_height :: Double
world_height = 700

-- Patterns for building ships
type BuildItem = ((Direction, String), (Part.Part, String))
type BuildPattern = (Ship.Ship, [BuildItem])

data Direction = U | D | L | R
    deriving (Eq, Show)

data Action = LClick (V2 Double) |
              RClick (V2 Double) |
              None |
              Step Double |
              InitRandom StdGen |
              AddRandomShip [BuildPattern] |
              AddShipRandomPos BuildPattern |
              AddRandomShipPos (V2 Double) [BuildPattern] |
              AddShip (V2 Double) BuildPattern
    deriving Show

data Model = Model { currentShip :: Int, -- The id of the current ship
                     ships :: Map.Map Int Ship.Ship,
                     shots :: Map.Map Int Shot.Shot,
                     parts :: Map.Map Int Part.Part,
                     nShips :: Int,
                     nShots :: Int,
                     nParts :: Int,
                     worldSize :: V2 Double,
                     gen :: StdGen}
    deriving Show

data Faction = Faction { factionId :: Int,
                         factioNName :: String}
    deriving Show

-- X position of the center, Y position of the center, Width, Height, Angle
type XCenter = Double
type YCenter = Double
type Width = Double
type Height = Double
type Angle = Double
data Rect = Rect XCenter YCenter Width Height Angle

gameFPS :: Int
gameFPS = 60
