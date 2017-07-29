module Lib
    (
        handlePhysics
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
