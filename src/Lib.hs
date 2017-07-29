module Lib
    (
        handlePhysics,
        doStep
    ) where

import qualified Data.Map as Map

import Model.Main

import Controller.Main
import qualified Controller.Shot as Shot
import qualified Controller.Ship as Ship

-- Do basic physics stuff like move and check collisions
handlePhysics :: Model -> Model
handlePhysics model = afterShips
    where afterShots = Map.foldl doPhysics model $ shots model
          afterShips = Map.foldl doPhysics afterShots $ ships afterShots

-- Do all the stuff that relies on the timestamp/being repeated.
doStep :: Double -> Model -> Model
doStep dt model = afterShips
    where afterShots = Map.foldl (handleStep dt) model $ shots model
          afterShips = Map.foldl (handleStep dt) afterShots $ ships afterShots
