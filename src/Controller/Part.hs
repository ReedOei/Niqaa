{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Controller.Part
    (
        Direction (..),
        add, place,
        canShoot, resetGun,
        getFarthest,
        getParts, getPartsById,
        buildShip,
        checkDestroyed
    ) where

import Data.List (find)
import qualified Data.List.Safe as Safe
import qualified Data.Map as Map
import Data.Ord (comparing)

import Linear.V2 (V2(V2))

import Helm.Color

import Model.Main
import qualified Model.Part as Part
import qualified Model.Ship as Ship
import qualified Model.Shot as Shot

import Controller.Main

import Misc

import System.Random
import System.IO.Unsafe

canShoot part@Part.Part{stats = Part.Gun{..}} = timer >= timerGoal && salvoTimer >= salvoTimerGoal
canShoot _ = False

isHull part@Part.Part{stats = Part.Hull} = True
isHull _ = False

isGun part@Part.Part{stats = Part.Gun{..}} = True
isGun _ = False

getAdjacentParts model part = checkCollisions part $ getPartsById model $ Part.shipId part

-- Should never be used on a non-gun, so let's make it fail if it is
resetGun part@Part.Part{Part.stats = stats@Part.Gun{..}}
    | shotsLeft == 0 = part {Part.stats = stats {Part.timer = 0, Part.salvoTimer = 0}}
    | otherwise = part {Part.stats = stats {Part.shotsLeft = shotsLeft - 1}}


buildShip :: Model -> BuildPattern -> Model
buildShip inModel@Model{nShips, ships} (inShip, buildParts) = foldl addParts model buildParts
    where model = inModel {ships = Map.insert (Ship.id ship) ship ships, nShips = nShips + 1}
          ship@Ship.Ship{Ship.pos, Ship.factionId, Ship.color}  = inShip {Ship.id = nShips}
          addParts model ((d, lookupName), (part, newName)) =
            case find (\Part.Part{Part.name} -> name == lookupName) $ getPartsById model nShips of
                Just neighbor -> place d (part {Part.name = newName}) model neighbor
                Nothing -> place U (part {Part.name = newName}) model (part {Part.pos = pos, Part.shipId = nShips, Part.factionId = factionId, Part.color = color})  -- Place it direction, this is the first piece

add :: V2 Double -> Part.Part -> Part.Part -> Model -> Model
add placePos part neighbor model@Model{..} =
    model {parts = Map.insert (nParts + 1) (part {Part.shipId = Part.shipId neighbor, Part.id = nParts + 1, Part.pos = placePos, Part.factionId = Part.factionId neighbor, Part.color = Part.color neighbor}) parts,
           nParts = nParts + 1}

getPartsById :: Model -> Int -> Map.Map Int Part.Part
getPartsById model@Model{parts} checkId = Map.filter (\Part.Part {Part.shipId} -> checkId == shipId) parts

getParts :: Model -> Ship.Ship -> Map.Map Int Part.Part
getParts model@Model{parts} ship = getPartsById model $ Ship.id ship

getFarthest :: Direction -> Model -> Ship.Ship -> Maybe Part.Part
getFarthest U model ship = Safe.minimumBy (comparing (\p -> getY (Part.pos p) - getY (Part.size p) / 2)) $ Map.elems $ getParts model ship
getFarthest D model ship = Safe.maximumBy (comparing (\p -> getY (Part.pos p) + getY (Part.size p) / 2)) $ Map.elems $ getParts model ship
getFarthest L model ship = Safe.minimumBy (comparing (\p -> getX (Part.pos p) - getX (Part.size p) / 2)) $ Map.elems $ getParts model ship
getFarthest R model ship = Safe.maximumBy (comparing (\p -> getX (Part.pos p) + getX (Part.size p) / 2)) $ Map.elems $ getParts model ship

place :: Direction -> Part.Part -> Model -> Part.Part -> Model
place U part model neighbor@Part.Part{Part.pos} = add (pos + V2 0 (-getY (Part.size part) / 2 - getY (Part.size neighbor) / 2)) part neighbor model
place D part model neighbor@Part.Part{Part.pos}  = add (pos + V2 0 (getY (Part.size part) / 2 + getY (Part.size neighbor) / 2)) part neighbor model
place R part model neighbor@Part.Part{Part.pos} = add (pos + V2 (getX (Part.size part / 2) + getX (Part.size neighbor) / 2) 0) part neighbor model
place L part model neighbor@Part.Part{Part.pos} = add (pos + V2 (-getX (Part.size part) / 2 - getX (Part.size neighbor) / 2) 0) part neighbor model

instance Physics Part.Part where
    getId = Part.id

    getBounds Part.Part{Part.pos = V2 x y, Part.stats = Part.Shield {..}} =
        Circle x y (shieldSize * strength / maxStrength)

    getBounds (Part.Part {Part.pos = V2 x y, Part.size = V2 w h, Part.clock}) = Rect x y w h 0

    doMove dt part@Part.Part{Part.pos, Part.vel} = part {Part.pos = pos + vel * pure dt}
    handleMove dt model@(Model {..}) part =
        model {parts = Map.insert (Part.id part) (doMove dt part) parts}

    handleCollisions model@(Model {..}) self = model

    handleStep dt inModel@Model{parts=inParts} self@Part.Part{..} =
        case enemies of
            [] -> model
            -- Try and shoot the enemies
            _ -> shoot model self $ Part.pos targetPart
        where enemies = filter (\check -> Ship.factionId check /= factionId) $ Map.elems $ ships inModel
              target = enemies !! n
              targetParts = Map.elems $ getParts inModel target
              targetPart = targetParts !! pid

              -- Generate random numbers like this so it remains pure
              (rtime, newGen1) = randomR (0, 1) $ gen inModel
              (n, newGen2) = randomR (0, length enemies - 1) newGen1
              (pid, newGen) = randomR (0, length targetParts - 1) newGen2

              -- This multiplier exists so that not all guns reload at exactly the same rate.
              multiplier = 1 / fromIntegral gameFPS
              model = inModel {parts = updatePhysics inParts $ newSelf {Part.clock = clock + dt}, gen = if null enemies then newGen1 else newGen}
              newSelf =
                case stats of
                    Part.Hull -> self

                    gun@Part.Gun{..} ->
                        if timer + dt > timerGoal && timer < timerGoal then
                            self {Part.stats = gun {Part.timer = timer + dt, Part.shotsLeft = salvoSize, Part.salvoTimer = 0} }
                        else if timer + dt > timerGoal then -- Don't increase both the timer
                            self {Part.stats = gun {Part.salvoTimer = salvoTimer + dt + rtime * multiplier}}
                        else
                            self {Part.stats = gun {Part.timer = timer + dt + rtime * multiplier, Part.salvoTimer = salvoTimer + dt + rtime * multiplier}}

                    shield@Part.Shield{..} ->
                        let newShield = shield {Part.strength = min (strength + dt * rechargeRate) maxStrength} in

                        if shieldFlashing then
                            if shieldFlashCurrent > shieldFlashSize then
                                self {Part.stats = newShield {Part.shieldFlashCurrent = 0, Part.shieldFlashing = False}}
                            else
                                self {Part.stats = newShield {Part.shieldFlashCurrent = shieldFlashCurrent + dt * shieldFlashSpeed}}
                        else
                            self {Part.stats = newShield}

-- Shoots a shot from the specified ship at the specified position.
shoot :: Model -> Part.Part -> V2 Double -> Model
shoot model@(Model {..}) self@Part.Part{Part.stats=stats@Part.Gun{..}, Part.color} targetPos
    | canShoot self = model {shots = Map.insert nShots newShot shots, nShots = nShots + 1,
                             parts = updatePhysics parts $ resetGun self,
                             gen = newGen}
    | otherwise = model
    where (miss, newGen) = randomR (-1, 1) gen
          newShot = Shot.Shot 
                    { 
                        Shot.id = nShots,
                        Shot.pos = Part.pos self,
                        Shot.vel = if Part.shotStats stats == Shot.Pulse then vel else 0,
                        Shot.shotSpeed = shotSpeed,
                        Shot.size = shotSize,
                        Shot.shotDamage = shotDamage,
                        Shot.shotColor = color,
                        Shot.stats = Part.shotStats stats,
                        Shot.factionId = Part.factionId self,
                        Shot.launchId = Part.shipId self
                    }
            where vel = (fromAngle $ angle perfect + miss * prec) * pure shotSpeed
                    where perfect = normalize (targetPos - Part.pos self)
shoot model _ _ = model -- If not a gun, we obviously can't shoot anything.

checkDestroyed :: Model -> Model
checkDestroyed model@Model{..} = model { parts = Map.filter ((> 0) . Part.health) parts }

