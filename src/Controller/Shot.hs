{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Controller.Shot
    (
        shot_size, shot_speed, shot_damage,
        create
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

import System.IO.Unsafe

shot_size :: Double
shot_size = 5
shot_speed = 1
shot_damage = 3

instance Physics Shot.Shot where
    getId = Shot.id
    getBounds (Shot.Shot {Shot.pos = V2 x y, Shot.size}) = Rect x y size size

    doMove shot@(Shot.Shot {..}) = shot {Shot.pos = pos + vel}
    handleMove model@(Model {..}) shot@(Shot.Shot {Shot.id}) = 
        model {shots = Map.insert id (doMove shot) shots}

    handleCollisions model@(Model {..}) shot@(Shot.Shot {..}) = 
        case concat $ Map.elems $ Map.map (checkCollisions shot . Ship.parts) ships of
            [] -> model
            collisions -> case find (\part -> Part.shipId part /= launchId) collisions of
                        Just part -> let newPart = Part.damage shot_damage part in
                            case Part.health newPart > 0 of
                                -- If it's still alive, just update it, which means we need to find the ship it comes from
                                True -> case Map.lookup (Part.shipId newPart) ships of
                                            Just ship -> 
                                                model {shots = Map.delete (Shot.id shot) shots,
                                                       ships = Map.insert (Ship.id ship) (ship {Ship.parts = Map.insert (getId newPart) newPart $ Ship.parts ship}) ships}
                                            Nothing -> error "Couldn't find parent ship of part."

                                -- Otherwise, get rid of it, because it's dead
                                False -> case Map.lookup (Part.shipId newPart) ships of
                                            Just ship@(Ship.Ship {..}) ->
                                                model {shots = Map.delete (Shot.id shot) shots,
                                                       ships = Map.insert (Ship.id ship) (ship {Ship.parts = Map.delete (getId newPart) $ Ship.parts ship}) ships}
                                            Nothing -> error "Couldn't find parent ship of part."
                        Nothing -> model

    handleStep dt model self = model

-- Shoots a shot from the specified ship at the specified position.
create :: Model -> Ship.Ship -> V2 Double -> Model
create model@(Model {..}) ship@(Ship.Ship {..}) targetPos = afterShot
    where gunPart = find (\part -> Part.partType part == "gun" && Part.timer part > Part.timeGoal part) $ Map.elems parts
          newShots nextGun = 
            Map.insert nShots (Shot.Shot nShots (Part.pos nextGun) vel shot_size (Ship.factionId ship) $ Ship.id ship) shots
            where vel = normalize (targetPos - Part.pos nextGun) * shot_speed
          afterShot = 
            case gunPart of 
                Just nextGun@Part.Part{..} -> 
                    case Map.lookup shipId ships of
                        Just ship@Ship.Ship{Ship.parts} -> model {shots = newShots nextGun, nShots = nShots + 1,
                                                                  ships = Map.insert shipId (ship {Ship.parts = updatePhysics parts $ nextGun {Part.timer = 0}}) ships}
                        Nothing -> error "Couldn't find parent ship of part."
                Nothing -> model

