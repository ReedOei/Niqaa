module Model.Main
    (
        Action (..),
        Model (..),
        Faction (..),
        Shape (..),
        Direction (..),
        Explosion (..),
        BuildItem, BuildPattern,
        world_width, world_height,
        gameFPS
    ) where

import qualified Data.Map as Map

import Linear.V2 (V2(V2))

import System.Random

import Helm.Color

import qualified Model.Part as Part
import qualified Model.Shot as Shot
import qualified Model.Ship as Ship
import Model.Action

import GUI.Main

world_width :: Double
world_width = 1200

world_height :: Double
world_height = 700

data Explosion = Explosion { explosionPos :: V2 Double,
                             maxSize :: Double, 
                             currentSize :: Double,
                             explosionColor :: Color,
                             expanding :: Bool }
    deriving Show

data Model = Model { currentShip :: Int, -- The id of the current ship
                     ships :: Map.Map Int Ship.Ship,
                     shots :: Map.Map Int Shot.Shot,
                     parts :: Map.Map Int Part.Part,
                     explosions :: [Explosion],
                     nShips :: Int,
                     nShots :: Int,
                     nParts :: Int,
                     worldSize :: V2 Double,
                     gen :: StdGen,
                     shipPatterns :: [BuildPattern],
                     zoomAmount :: Double,
                     clock :: Double,
                     lastTicks :: [Double],
                     guiManager :: GUIManager}
    deriving Show

data Faction = Faction { factionId :: Int,
                         factioNName :: String}
    deriving Show

gameFPS :: Int
gameFPS = 100

