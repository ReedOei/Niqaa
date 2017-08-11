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
import Model.Physics

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
