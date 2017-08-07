module Model.Action
    (
        Action (..),
        Direction (..),
        Shape (..),
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
              LoadPatterns String |
              LoadedPatterns [BuildPattern] |
              ReloadPatterns |
              AddRandomShip |
              AddRandomShipPos (V2 Double) |
              AddShip (V2 Double) BuildPattern
    deriving Show

type XCenter = Double
type YCenter = Double
type Width = Double
type Height = Double
type Angle = Double
type Radius = Double

data Shape = Rect XCenter YCenter Width Height |
             Circle XCenter YCenter Radius


