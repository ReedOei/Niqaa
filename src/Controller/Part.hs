{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Controller.Part
    (
        Direction (..),
        base, gun, makeGun,
        add, place,
        canShoot, resetTimer,
        getFarthest,
        getParts, getPartsById,
        buildShip
    ) where

import Data.List (find)
import qualified Data.List.Safe as Safe
import qualified Data.Map as Map
import Data.Ord (comparing)

import Linear.V2 (V2(V2))

import Model.Main
import qualified Model.Part as Part
import qualified Model.Ship as Ship
import qualified Model.Shot as Shot

import Controller.Main

import Misc

import System.Random
import System.IO.Unsafe

base :: Part.Part
base = Part.Part (-1) (-1) (V2 0 0) (V2 0 0) 50 20 (-1) Part.Hull

gun :: Part.Part
gun = makeGun 5 5 $ Part.Gun 5 2000 0 3 5 1

makeGun :: Double -> Double -> Part.Stats -> Part.Part
makeGun health size stats = Part.Part (-1) (-1) (V2 0 0) (V2 0 0) health size (-1) stats

canShoot part@Part.Part{stats = Part.Gun _ goal timer _ _ _} = timer > goal
canShoot _ = False

-- Should never be used on a non-gun, so let's make it fail if it is
resetTimer part@Part.Part{stats = Part.Gun prec goal _ size damage speed} = part {Part.stats = Part.Gun prec goal 0 size damage speed}

type BuildPattern = (Ship.Ship, [(Direction, Part.Part)])

buildShip :: Model -> BuildPattern -> Model
buildShip inModel@Model{nShips, ships} (inShip, parts) = foldl (\m (d, p) -> place d p m ship) model parts
    where model = inModel {ships = Map.insert (Ship.id ship) ship ships, nShips = nShips + 1}
          ship = inShip {Ship.id = nShips} 

add :: V2 Double -> Part.Part -> Ship.Ship -> Model -> Model
add placePos part ship model@Model{..} = 
    model {parts = Map.insert (nParts + 1) (part {Part.shipId = Ship.id ship, Part.id = nParts + 1, Part.pos = placePos, Part.factionId = Ship.factionId ship}) parts, 
           nParts = nParts + 1}

getPartsById :: Model -> Int -> Map.Map Int Part.Part
getPartsById model@Model{parts} checkId = Map.filter (\Part.Part {Part.shipId} -> checkId == shipId) parts

getParts :: Model -> Ship.Ship -> Map.Map Int Part.Part
getParts model@Model{parts} ship@Ship.Ship{Ship.id} = getPartsById model id

getFarthest :: Direction -> Model -> Ship.Ship -> Maybe Part.Part
getFarthest U model ship = Safe.minimumBy (comparing (\p -> getY (Part.pos p) - Part.size p / 2)) $ Map.elems $ getParts model ship
getFarthest D model ship = Safe.maximumBy (comparing (\p -> getY (Part.pos p) + Part.size p / 2)) $ Map.elems $ getParts model ship
getFarthest L model ship = Safe.minimumBy (comparing (\p -> getX (Part.pos p) - Part.size p / 2)) $ Map.elems $ getParts model ship
getFarthest R model ship = Safe.maximumBy (comparing (\p -> getX (Part.pos p) + Part.size p / 2)) $ Map.elems $ getParts model ship

place :: Direction -> Part.Part -> Model -> Ship.Ship -> Model
place U part model ship = 
    case getFarthest U model ship of
        Just neighbor@(Part.Part {Part.pos}) -> 
            add (pos + V2 0 (-Part.size part / 2 - Part.size neighbor / 2)) part ship model
        Nothing -> add (Ship.pos ship) part ship model

place D part model ship = 
    case getFarthest D model ship of
        Just neighbor@(Part.Part {Part.pos}) ->
            add (pos + V2 0 (Part.size part / 2 + Part.size neighbor / 2)) part ship model
        Nothing -> add (Ship.pos ship) part ship model

place R part model ship  = 
    case getFarthest R model ship of
        Just neighbor@(Part.Part {Part.pos}) ->
            add (pos + V2 (Part.size part / 2 + Part.size neighbor / 2) 0) part ship model
        Nothing -> add (Ship.pos ship) part ship model

place L part model ship  = 
    case getFarthest L model ship of
        Just neighbor@(Part.Part {Part.pos}) ->
            add (pos + V2 (-Part.size part / 2 - Part.size neighbor / 2) 0) part ship model
        Nothing -> add (Ship.pos ship) part ship model

instance Physics Part.Part where
    getId = Part.id
    getBounds (Part.Part {Part.pos = V2 x y, Part.size}) = Rect x y size size

    doMove part@Part.Part{Part.pos, Part.vel} = part {Part.pos = pos + vel}
    handleMove model@(Model {..}) part = 
        model {parts = Map.insert (Part.id part) (doMove part) parts}

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
              (n, newGen1) = randomR (0, length enemies - 1) $ gen inModel
              (pid, newGen) = randomR (0, length targetParts - 1) newGen1
              model = inModel {parts = updatePhysics inParts newSelf, gen = newGen}
              newSelf = 
                case stats of
                    Part.Hull -> self
                    Part.Gun prec goal timer size damage speed -> self {Part.stats = Part.Gun prec goal (timer + dt) size damage speed}

-- Shoots a shot from the specified ship at the specified position.
shoot :: Model -> Part.Part -> V2 Double -> Model
shoot model@(Model {..}) self@Part.Part{stats = Part.Gun prec _ _ shotSize shotDamage shotSpeed} targetPos
    | canShoot self = model {shots = Map.insert nShots newShot shots, nShots = nShots + 1,
                             parts = updatePhysics parts $ resetTimer self,
                             gen = newGen}
    | otherwise = model
    where (miss, newGen) = randomR (-1, 1) gen 
          newShot = Shot.Shot nShots (Part.pos self) vel shotSize shotDamage (Part.factionId self) (Part.shipId self) 
            where vel = (fromAngle $ angle perfect + miss * prec) * pure shotSpeed
                    where perfect = normalize (targetPos - Part.pos self)
shoot model _ _ = model -- If not a gun, we obviously can't shoot anything.

