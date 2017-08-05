module Model.Action
    (
        Action (..),
        Direction (..),
        Rect (..),
        BuildItem, BuildPattern
    ) where

import Linear.V2 (V2(V2))

import System.Random

import qualified Model.Part as Part
import qualified Model.Ship as Ship

data Direction = U | D | L | R
    deriving (Eq, Show, Read)

-- Patterns for building ships
type BuildItem = ((Direction, String), (Part.Part, String))
type BuildPattern = (Ship.Ship, [BuildItem])

data Action = LClick (V2 Double) | 
              RClick (V2 Double) | 
              None | 
              Step Double |
              InitRandom StdGen |
              LoadPatterns [BuildPattern] |
              AddRandomShip |
              AddRandomShipPos (V2 Double) |
              AddShip (V2 Double) BuildPattern
    deriving Show

-- X position of the center, Y position of the center, Width, Height, Angle
type XCenter = Double
type YCenter = Double
type Width = Double
type Height = Double
type Angle = Double
data Rect = Rect XCenter YCenter Width Height

