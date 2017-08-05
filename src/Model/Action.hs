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
    deriving (Eq, Show)

-- Patterns for building ships
type BuildItem = ((Direction, String), (Part.Part, String))
type BuildPattern = (Ship.Ship, [BuildItem])

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

data Rect = Rect Double Double Double Double

