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

addExplosion :: Shot.Shot -> Model -> V2 Double -> Model
addExplosion shot@Shot.Shot{Shot.shotColor} model@Model{explosions} pos = model {explosions = newExplosion : explosions }
    where newExplosion = Explosion
                         {
                            explosionPos = pos,
                            maxSize = 10,
                            explosionColor = shotColor,
                            currentSize = 0,
                            expanding = True
                         }

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
    getBounds (Shot.Shot {Shot.pos = V2 x y, Shot.size = V2 w h, Shot.vel, Shot.stats}) = 
        case stats of
            Shot.Missile{..} -> Rect x y w h (a * pi / 180)
            _ -> Rect x y w h 0
        where a = angle vel

    doMove dt shot@(Shot.Shot {..}) = shot {Shot.pos = pos + vel * pure dt}
    handleMove dt model@(Model {..}) shot@(Shot.Shot {Shot.id}) =
        model {shots = Map.insert id (doMove dt shot) shots}

    handleCollisions model@(Model {..}) shot@(Shot.Shot {..}) =
        case checkCollisions shot parts of
            [] -> model
            collisions ->
                case find (\part -> Part.factionId part /= factionId) collisions of
                    Just part -> let (newSelf, newPart) = damage shot part in
                        let newModel =  
                                model {shots = Map.insert (Shot.id shot) newSelf shots,
                                       parts = Map.insert (getId newPart) newPart parts}
                        in
                            case stats of
                                Shot.Missile{..} -> addExplosion shot newModel pos
                                _ -> newModel
                    Nothing -> model

    -- Delete ourselves if we're out of bounds
    handleStep dt model@Model{worldSize = V2 w h, shots, parts, gen} self@Shot.Shot{..}
        | x < 0 || y < 0 || x >= w || y >= h = model {shots = Map.delete id shots}
        | otherwise = 
            case Shot.stats self of
                -- If we're a missile, try to find a target and re-aim ourselves towards it.
                stats@Shot.Missile{..} ->
                    if missileFuel <= 0 then 
                        model
                    else 
                        case find (\part -> Part.factionId part /= factionId) $ Map.elems parts of
                            Nothing -> model
                            Just target@Part.Part{Part.pos = targetPos} -> 
                                let (newSelf, newGen) = getMissileMovement stats targetPos in
                                    model {gen = newGen, 
                                           shots = updatePhysics shots newSelf}
                _ -> model
        where (V2 x y) = pos
              getMissileMovement stats@Shot.Missile{..} targetPos = (newSelf, newGen)
                  where newSelf = self {Shot.stats = stats {Shot.missileFuel = missileFuel - dt}, Shot.vel = newVel}
                        (miss, newGen) = randomR (-1, 1) gen
                        newVel
                            | shotSpeed^2 < distanceSquared acc = vel
                            | otherwise = acc 
                            where perfect = normalize (targetPos - pos)
                                  acc = vel + pure missileAcceleration * (fromAngle $ angle perfect + miss)

checkDestroyed :: Model -> Model
checkDestroyed model@Model{..} = model { shots = Map.filter ((> 0) . Shot.shotDamage) shots }

