{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Controller.Ship
    (
        getCurrent,
        findAt,
        checkDestroyed
    ) where

import Data.List (find)
import qualified Data.Map as Map

import Linear.V2 (V2(V2))

import Model.Main
import qualified Model.Ship as Ship
import qualified Model.Part as Part

import Controller.Main
import qualified Controller.Part as Part
import qualified Controller.Shot as Shot

instance Physics Ship.Ship where
    getId = Ship.id
    getBounds (Ship.Ship {Ship.pos = V2 x y}) = Rect x y 20 20
    
    -- Moving all the ship's parts is what counts as moving.
    doMove ship@(Ship.Ship {..}) = 
        ship {Ship.parts = Map.map doMove parts, 
              -- We don't want to get the average of our ship parts if there aren't any because then we'll divide by 0.
              -- The ship will be removed next step, so we just need to leave it as is.
              Ship.pos = case Map.elems parts of
                            [] -> pos
                            parts -> average $ map Part.pos parts}
    handleMove model@(Model {..}) ship = 
        model {ships = Map.insert (Ship.id ship) (doMove ship) ships}

    handleCollisions model@(Model {..}) self = model

    handleStep dt model self@Ship.Ship{..} = 
        case find (\check -> Ship.factionId check /= factionId) $ ships afterParts of
            Just target -> 
                -- Relookup ourselves to make sure we have the latest version.
                case Map.lookup (Ship.id self) $ ships afterParts of
                    Just ship -> Shot.create afterParts ship $ Part.pos $ head $ Map.elems $ Ship.parts target
                    Nothing -> afterParts
            Nothing -> afterParts
        where afterParts = Map.foldl (handleStep dt) model parts
    
getCurrent :: Model -> Maybe Ship.Ship
getCurrent (Model {..}) = Map.lookup currentShip ships

findAt :: V2 Double -> Map.Map k Ship.Ship -> Maybe Int
findAt pos ships = getId <$> (find ((pos `inRect`) . getBounds) $ map snd $ Map.toList ships)

checkDestroyed :: Model -> Model
checkDestroyed model@(Model {..}) = model {ships = Map.filter (not . Map.null . Ship.parts) ships}

