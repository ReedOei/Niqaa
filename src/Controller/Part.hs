{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Controller.Part
    (
        Direction (..),
        base, gun,
        add, place,
        damage
    ) where

import qualified Data.List.Safe as Safe
import qualified Data.Map as Map
import Data.Ord (comparing)

import Linear.V2 (V2(V2))

import Model.Main
import qualified Model.Part as Part
import qualified Model.Ship as Ship

import Controller.Main

import System.IO.Unsafe

base :: Part.Part
base = Part.Part (-1) (-1) "hull" (V2 0 0) (V2 0 0) 100 20 0 0

gun :: Part.Part
gun = Part.Part (-1) (-1) "gun" (V2 0 0) (V2 0 0) 50 10 2000 0

add :: V2 Double -> Part.Part -> Ship.Ship -> Ship.Ship
add placePos part ship@(Ship.Ship {..}) = ship {Ship.parts = Map.insert (nParts + 1) (part {Part.shipId = id, Part.id = nParts + 1, Part.pos = placePos}) parts, 
                                                 Ship.nParts = nParts + 1}

place :: Direction -> Part.Part -> Ship.Ship -> Ship.Ship
place U part ship@(Ship.Ship {..}) = 
    case Safe.minimumBy (comparing (\Part.Part {Part.pos = V2 x y} -> y)) $ Map.elems parts of
        Just neighbor@(Part.Part {Part.pos}) -> 
            add (pos + V2 0 (-Part.size part / 2 - Part.size neighbor / 2)) part ship
        Nothing -> add (Ship.pos ship) part ship

place D part ship@(Ship.Ship {..}) = 
    case Safe.maximumBy (comparing (\Part.Part {Part.pos = V2 x y} -> y)) $ Map.elems parts of
        Just neighbor@(Part.Part {Part.pos}) ->
            add (pos + V2 0 (Part.size part / 2 + Part.size neighbor / 2)) part ship
        Nothing -> add (Ship.pos ship) part ship

place R part ship@(Ship.Ship {..}) = 
    case Safe.maximumBy (comparing (\Part.Part {Part.pos = V2 x y} -> x)) $ Map.elems parts of
        Just neighbor@(Part.Part {Part.pos}) ->
            add (pos + V2 (Part.size part / 2 + Part.size neighbor / 2) 0) part ship
        Nothing -> add (Ship.pos ship) part ship

place L part ship@(Ship.Ship {..}) = 
    case Safe.minimumBy (comparing (\Part.Part {Part.pos = V2 x y} -> x)) $ Map.elems parts of
        Just neighbor@(Part.Part {Part.pos}) ->
            add (pos + V2 (-Part.size part / 2 - Part.size neighbor / 2) 0) part ship
        Nothing -> add (Ship.pos ship) part ship

instance Physics Part.Part where
    getId = Part.id
    getBounds (Part.Part {Part.pos = V2 x y, Part.size}) = Rect x y size size

    doMove part@Part.Part{Part.pos, Part.vel} = part {Part.pos = pos + vel}
    handleMove model part = model -- Parts aren't part of the model. Their movement is handled when the ship moves.

    handleCollisions model@(Model {..}) self = model

    handleStep dt model@Model{..} self@Part.Part{..} = 
        case Map.lookup shipId ships of
            Just ship@Ship.Ship{Ship.parts} -> 
                model {ships = Map.insert shipId (ship {Ship.parts = updatePhysics parts (self {Part.timer = timer + dt})}) ships}
            Nothing -> error "Part has no parent ship!"

damage :: Int -> Part.Part -> Part.Part
damage amount part@(Part.Part {..}) = part {Part.health = health - amount}

