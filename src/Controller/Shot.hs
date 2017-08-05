{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Controller.Shot
    (
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

damage amount part@(Part.Part {..}) = part {Part.health = health - amount}

instance Physics Shot.Shot where
    getId = Shot.id
    getBounds (Shot.Shot {Shot.pos = V2 x y, Shot.size}) = Rect x y size size

    doMove shot@(Shot.Shot {..}) = shot {Shot.pos = pos + vel}
    handleMove model@(Model {..}) shot@(Shot.Shot {Shot.id}) = 
        model {shots = Map.insert id (doMove shot) shots}

    handleCollisions model@(Model {..}) shot@(Shot.Shot {..}) = 
        case checkCollisions shot parts of
            [] -> model
            collisions -> 
                case find (\part -> Part.factionId part /= factionId) collisions of
                    Just part -> let newPart = damage shotDamage part in
                        case Part.health newPart > 0 of
                            -- If it's still alive, just update it, which means we need to find the ship it comes from
                            True -> model {shots = Map.delete (Shot.id shot) shots,
                                           parts = Map.insert (getId newPart) newPart parts}

                            -- Otherwise, get rid of it, because it's dead
                            False -> model {shots = Map.delete (Shot.id shot) shots,
                                            parts = Map.delete (getId newPart) parts}
                    Nothing -> model
    
    -- Delete ourselves if we're out of bounds
    handleStep dt model@Model{worldSize = V2 w h, shots} self@Shot.Shot{Shot.id, Shot.pos = V2 x y}
        | x < 0 || y < 0 || x >= w || y >= h = model {shots = Map.delete id shots}
        | otherwise = model

