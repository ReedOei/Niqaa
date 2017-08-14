{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

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

import System.IO.Unsafe

-- Do basic physics stuff like move and check collisions
handlePhysics :: Double -> Model -> Model
handlePhysics dt model = afterShips
    where afterShots = Map.foldl (doPhysics dt) model $ shots model
          afterParts = Map.foldl (doPhysics dt) afterShots $ parts afterShots
          afterShips = Map.foldl (doPhysics dt) afterParts $ ships afterParts

handleExplosions :: Double -> Model -> Model
handleExplosions dt model@Model{explosions} =
    model {explosions = map go explosions}
    where go explosion@Explosion{..}
            | expanding && currentSize < maxSize = explosion {currentSize = currentSize + 100 * dt}
            | expanding && currentSize >= maxSize = explosion {expanding = False}
            | not expanding && currentSize >= 0 = explosion {currentSize = currentSize - 100 * dt}
            | otherwise = explosion -- Should be destroyed soon, so let's just do nothing

-- Do all the stuff that relies on the timestamp/being repeated.
doStep :: Double -> Model -> Model
doStep dt model = afterExplosions
    where afterShots = Map.foldl (handleStep dt) model $ shots model
          afterParts = Map.foldl (handleStep dt) afterShots $ parts afterShots
          afterShips = Map.foldl (handleStep dt) afterParts $ ships afterParts
          afterExplosions = handleExplosions dt afterShips

checkDestroyedExplosion :: Model -> Model
checkDestroyedExplosion model@Model{explosions} =
    model {explosions = filter (\exp -> currentSize exp >= 0) explosions}

checkDestroyed :: Model -> Model
checkDestroyed model = Ship.checkDestroyed $ 
                       Part.checkDestroyed $ 
                       Shot.checkDestroyed $ 
                       checkDestroyedExplosion model 

