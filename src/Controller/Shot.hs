{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Controller.Shot
    (
        checkDestroyed
    ) where

import Data.List (find)
import qualified Data.Map as Map

import Linear.V2 (V2(V2))

import Model.Main
import qualified Model.Shot as Shot
import qualified Model.Part as Part
import qualified Model.Ship as Ship

import Controller.Main
import qualified Controller.Part as Part

import Misc

import System.IO.Unsafe
import System.Random

damage self@Shot.Shot{Shot.shotDamage} shield@(Part.Part {Part.health, Part.stats = stats@Part.Shield{..}})
    | strength > shotDamage = (newSelf, shield {Part.stats = stats {Part.strength = strength - shotDamage, Part.shieldFlashing = True, Part.shieldFlashCurrent = 0}})
    | otherwise = (newSelf,
                   shield
                   {
                    Part.health = health - (shotDamage - strength),
                    Part.stats = stats {Part.strength = 1, Part.shieldFlashing = True, Part.shieldFlashCurrent = 0}
                   })
    where newSelf = self {Shot.shotDamage = shotDamage - (health + strength) }

damage self@Shot.Shot{Shot.shotDamage} part@(Part.Part {..}) =
    (newSelf, part {Part.health = health - shotDamage})
    where newSelf = self {Shot.shotDamage = shotDamage - health }

instance Physics Shot.Shot where
    getId = Shot.id
    getBounds (Shot.Shot {Shot.pos = V2 x y, Shot.size}) = Rect x y size size 0

    doMove dt shot@(Shot.Shot {..}) = shot {Shot.pos = pos + vel * pure dt}
    handleMove dt model@(Model {..}) shot@(Shot.Shot {Shot.id}) =
        model {shots = Map.insert id (doMove dt shot) shots}

    handleCollisions model@(Model {..}) shot@(Shot.Shot {..}) =
        case checkCollisions shot parts of
            [] -> model
            collisions ->
                case find (\part -> Part.factionId part /= factionId) collisions of
                    Just part -> let (newSelf, newPart) = damage shot part in
                        model {shots = Map.insert (Shot.id shot) newSelf shots,
                               parts = Map.insert (getId newPart) newPart parts}
                    Nothing -> model

    -- Delete ourselves if we're out of bounds
    handleStep dt model@Model{worldSize = V2 w h, shots} self@Shot.Shot{Shot.id, Shot.pos = V2 x y}
        | x < 0 || y < 0 || x >= w || y >= h = model {shots = Map.delete id shots}
        | otherwise = model

checkDestroyed :: Model -> Model
checkDestroyed model@Model{..} = model { shots = Map.filter ((> 0) . Shot.shotDamage) shots }
