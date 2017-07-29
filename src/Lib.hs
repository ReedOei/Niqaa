{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( 
      findShipAt, getCurrentShip, checkDestroyedShips,
      createShot,
      handlePhysics,
      Ship (..),
      Shot (..), shot_size, shot_speed,
      Model (..),
      Action (..),
      Part (..), addPart, basePart, placePart,
      Direction (..)
    ) where

import Linear.V2 (V2(V2), ey, ex)

import Data.List (find)
import qualified Data.List.Safe as Safe
import qualified Data.Map as Map
import Data.Ord (comparing)

import System.IO.Unsafe

average :: Floating a => [a] -> a
average xs = sum xs / (fromIntegral $ length xs)

shot_size :: Double
shot_size = 5
shot_speed = 1
shot_damage = 3

data Direction = U | D | L | R

basePart :: Part
basePart = Part (-1) (-1) "hull" (V2 0 0) (V2 0 0) 10 20

addPart :: V2 Double -> Part -> Ship -> Ship
addPart pos part@(Part {..}) ship@(Ship {..}) = ship {shipParts = Map.insert (nParts + 1) (part {parentShipId = shipId, partId = nParts + 1, partPos = pos}) shipParts, 
                                                 nParts = nParts + 1}

placePart :: Direction -> Part -> Ship -> Ship
placePart U part ship@(Ship {..}) = 
    case Safe.minimumBy (comparing (\Part {partPos = V2 x y} -> y)) $ Map.elems shipParts of
        Just neighbor@(Part {partPos}) -> 
            addPart (partPos + V2 0 (-partSize part / 2 - partSize neighbor / 2)) part ship
        Nothing -> addPart shipPos part ship

placePart D part ship@(Ship {..}) = 
    case Safe.maximumBy (comparing (\Part {partPos = V2 x y} -> y)) $ Map.elems shipParts of
        Just neighbor@(Part {partPos}) ->
            addPart (partPos + V2 0 (partSize part / 2 + partSize neighbor / 2)) part ship
        Nothing -> addPart shipPos part ship

placePart R part ship@(Ship {..}) = 
    case Safe.maximumBy (comparing (\Part {partPos = V2 x y} -> x)) $ Map.elems shipParts of
        Just neighbor@(Part {partPos}) ->
            addPart (partPos + V2 (partSize part / 2 + partSize neighbor / 2) 0) part ship
        Nothing -> addPart shipPos part ship

placePart L part ship@(Ship {..}) = 
    case Safe.minimumBy (comparing (\Part {partPos = V2 x y} -> x)) $ Map.elems shipParts of
        Just neighbor@(Part {partPos}) ->
            addPart (partPos + V2 (-partSize part / 2 - partSize neighbor / 2) 0) part ship
        Nothing -> addPart shipPos part ship

data Action = LClick (V2 Double) | RClick (V2 Double) | None | Step Double
    deriving Show
data Model = Model { currentShip :: Int, -- The id of the current ship
                     ships :: Map.Map Int Ship,
                     shots :: Map.Map Int Shot,
                     nShips :: Int, 
                     nShots :: Int}
    deriving Show

data Faction = Faction { factionId :: Int,
                         factioNName :: String}
    deriving Show

data Part = Part { partId :: Int,
                   parentShipId :: Int,
                   partType :: String,
                   partPos :: V2 Double,
                   partVel :: V2 Double,
                   health :: Int,
                   partSize :: Double}
    deriving Show

data Ship = Ship { shipId :: Int,
                   shipName :: String,
                   shipFaction :: Int,
                   shipPos :: V2 Double,
                   shipParts :: Map.Map Int Part,
                   nParts :: Int}
    deriving (Show)

data Shot = Shot { shotId :: Int,
                   shotPos :: V2 Double,
                   shotVel :: V2 Double,
                   launchId :: Int}
    deriving (Show)

data Rect = Rect Double Double Double Double

class Physics o where
    getId :: o -> Int

    doMove :: o -> o
    handleMove :: Model -> o -> Model
    
    getBounds :: o -> Rect

    handleCollisions :: Model -> o -> Model

    handleStep :: Model -> o -> Double -> Model

    checkCollisions :: Physics a => o -> Map.Map k a -> [a]
    checkCollisions self m = filter (collided self) $ Map.elems m

    collided :: Physics a => o -> a -> Bool
    collided a b = collide (getBounds a) (getBounds b)

    doPhysics :: Model -> o -> Model
    doPhysics model self = handleCollisions (handleMove model self) (doMove self)

instance Physics Shot where
    getId = shotId
    getBounds (Shot {shotPos = V2 x y}) = Rect x y shot_size shot_size

    doMove shot@(Shot {..}) = shot {shotPos = shotPos + shotVel}
    handleMove model@(Model {..}) shot@(Shot {..}) = 
        model {shots = Map.insert shotId (doMove shot) shots}

    handleCollisions model@(Model {..}) shot@(Shot {..}) = 
        case concat $ Map.elems $ Map.map (checkCollisions shot . shipParts) ships of
            [] -> model
            collisions -> case find (\part -> parentShipId part /= launchId) collisions of
                        Just part -> let newPart = damage shot_damage part in
                            case health newPart > 0 of
                                -- If it's still alive, just update it, which means we need to find the ship it comes from
                                True -> case Map.lookup (parentShipId newPart) ships of
                                            Just ship@(Ship{..}) -> 
                                                model {shots = Map.delete shotId shots,
                                                       ships = Map.insert shipId (ship {shipParts = Map.insert (getId newPart) newPart shipParts}) ships}
                                            Nothing -> error "Couldn't find parent ship of part."

                                -- Otherwise, get rid of it, because it's dead
                                False -> case Map.lookup (parentShipId newPart) ships of
                                            Just ship@(Ship {..}) ->
                                                model {shots = Map.delete shotId shots,
                                                       ships = Map.insert shipId (ship {shipParts = Map.delete (getId newPart) shipParts}) ships}
                                            Nothing -> error "Couldn't find parent ship of part."
                        Nothing -> model

instance Physics Part where
    getId = partId
    getBounds (Part {partPos = V2 x y, partSize=partSize}) = Rect x y partSize partSize

    doMove part@(Part {..}) = part {partPos = partPos + partVel}
    handleMove model@(Model {..}) part@(Part {..}) = model -- Parts aren't part of the model. Their movement is handled when the ship moves.

    handleCollisions model@(Model {..}) self = model

instance Physics Ship where
    getId = shipId
    getBounds (Ship {shipPos = V2 x y}) = Rect x y 20 20
    
    -- Moving all the ship's parts is what counts as moving.
    doMove ship@(Ship {..}) = 
        ship {shipParts = Map.map doMove shipParts, 
              -- We don't want to get the average of our ship parts if there aren't any because then we'll divide by 0.
              -- The ship will be removed next step, so we just need to leave it as is.
              shipPos = case Map.elems shipParts of
                            [] -> shipPos
                            parts -> average $ map partPos parts}
    handleMove model@(Model {..}) ship@(Ship {..}) = 
        model {ships = Map.insert shipId (doMove ship) ships}

    handleCollisions model@(Model {..}) self = model
    
setAt :: [a] -> Int -> a -> [a]
setAt xs i e = take i xs ++ [e] ++ drop (i + 1) xs

v2Len :: Floating a => V2 a -> a
v2Len (V2 x y) = sqrt $ x^2 + y^2

normalize :: Floating a => V2 a -> V2 a
normalize v = v / (pure $ v2Len v)

-- x and y are the position of the center
inRect :: V2 Double -> Rect -> Bool
inRect (V2 x1 y1) (Rect x2 y2 w h) = x1 `inRange` (x2 - w, x2 + w) && y1 `inRange` (y2 - h, y2 + h)
    where inRange v (l, h) = v >= l && v <= h

collide :: Rect -> Rect -> Bool
collide (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) = 
    x1 - w1 < x2 + w2 &&
    x1 + w1 > x2 - w2 &&
    y1 - h1 < y2 + h2 &&
    y1 + h1 > y2 - h2

findShipAt :: V2 Double -> Map.Map k Ship -> Maybe Int
findShipAt pos ships = getId <$> (find ((pos `inRect`) . getBounds) $ map snd $ Map.toList ships)

getCurrentShip :: Model -> Maybe Ship
getCurrentShip (Model {..}) = Map.lookup currentShip ships

damage :: Int -> Part -> Part
damage amount part@(Part {..}) = part {health = health - amount}

-- Shoots a shot from the specified ship at the specified position.
createShot :: Model -> Ship -> V2 Double -> Model
createShot model@(Model {..}) ship@(Ship {..}) pos = model {shots=newShots, nShots = nShots + 1}
    where nextGun = head $ Map.elems shipParts -- TODO: Make it so that not every piece shoots.
          newShots = Map.insert nShots (Shot nShots (partPos nextGun) vel shipId) shots
            where vel = normalize (pos - partPos nextGun) * shot_speed

handleShots :: Model -> Model
handleShots initModel = foldl handleShot initModel $ shots initModel
    where handleShot model@(Model {..}) shot@(Shot {..}) = model {shots=newShots}
            where newShots = Map.insert shotId newShot shots
                  newShot = shot {shotPos = shotPos + shotVel}

-- Do basic physics stuff like move and check collisions
handlePhysics :: Model -> Model
handlePhysics model = afterShips
    where afterShots = Map.foldl doPhysics model $ shots model
          afterShips = Map.foldl doPhysics afterShots $ ships afterShots

checkDestroyedShips :: Model -> Model
checkDestroyedShips model@(Model {..}) = model {ships = Map.filter (not . Map.null . shipParts) ships}

