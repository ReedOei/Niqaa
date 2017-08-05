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

import System.Random (randomR)

import Model.Main
import qualified Model.Ship as Ship
import qualified Model.Part as Part

import Controller.Main
import qualified Controller.Part as Part

import Misc

import System.IO.Unsafe

instance Physics Ship.Ship where
    getId = Ship.id
    getBounds (Ship.Ship {Ship.pos = V2 x y}) = Rect x y 20 20

    doMove = id -- We do the actual moving in handleMove

    handleMove model@(Model {..}) ship =
        model {ships = Map.insert (Ship.id ship) newShip ships}
        where newShip =
              -- We don't want to get the average of our ship parts if there aren't any because then we'll divide by 0.
              -- The ship will be removed next step, so we just need to leave it as is.
                ship {Ship.pos = case Map.elems $ Part.getParts model ship of
                                    [] -> Ship.pos ship
                                    parts -> average $ map Part.pos parts}

    handleCollisions model@(Model {..}) self = model

    handleStep dt model self@Ship.Ship{..} = model

getCurrent :: Model -> Maybe Ship.Ship
getCurrent (Model {..}) = Map.lookup currentShip ships

findAt :: V2 Double -> Map.Map k Ship.Ship -> Maybe Int
findAt pos ships = getId <$> (find ((pos `inRect`) . getBounds) $ map snd $ Map.toList ships)

checkDestroyed :: Model -> Model
checkDestroyed model@(Model {..}) = model {ships = Map.filter (not . Map.null . Part.getParts model) ships}

