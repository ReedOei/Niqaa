module Lib
    (
        handlePhysics,
        doStep,
        checkDestroyed
    ) where

import qualified Data.Map as Map

import Model.Main

import Controller.Main
import qualified Controller.Shot as Shot
import qualified Controller.Ship as Ship
import qualified Controller.Part as Part

-- Do basic physics stuff like move and check collisions
handlePhysics :: Double -> Model -> Model
handlePhysics dt model = afterShips
    where afterShots = Map.foldl (doPhysics dt) model $ shots model
          afterParts = Map.foldl (doPhysics dt) afterShots $ parts afterShots
          afterShips = Map.foldl (doPhysics dt) afterParts $ ships afterParts

-- Do all the stuff that relies on the timestamp/being repeated.
doStep :: Double -> Model -> Model
doStep dt model = afterShips
    where afterShots = Map.foldl (handleStep dt) model $ shots model
          afterParts = Map.foldl (handleStep dt) afterShots $ parts afterShots
          afterShips = Map.foldl (handleStep dt) afterParts $ ships afterParts

checkDestroyed :: Model -> Model
checkDestroyed model = Ship.checkDestroyed $ 
                       Part.checkDestroyed $ 
                       Shot.checkDestroyed model 

