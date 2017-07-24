{-# LANGUAGE RecordWildCards #-}

module Lib
    ( 
      findShipAt, getCurrentShip,
      createShot,
      handlePhysics,
      Ship (..),
      Shot (..), shot_size, shot_speed,
      Model (..),
      Action (..),
    ) where

import Linear.V2 (V2(V2))

import Data.List (find)
import qualified Data.Map as Map

import System.IO.Unsafe

shot_size :: Double
shot_size = 5
shot_speed = 1
shot_damage = 3

data Action = LClick (V2 Double) | RClick (V2 Double) | None | Step Double
    deriving Show
data Model = Model { currentShip :: Int, -- The id of the current ship
                     ships :: Map.Map Int Ship,
                     shots :: Map.Map Int Shot,
                     nShips :: Int, 
                     nShots :: Int}
    deriving Show

data Ship = Ship { shipId :: Int,
                   shipName :: String,
                   shipSize :: Double,
                   shipPos :: V2 Double,
                   shipVel :: V2 Double,
                   health :: Int}
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
        case checkCollisions shot ships of
            [] -> model
            collisions -> case find (\ship -> getId ship /= launchId) collisions of
                        Just ship -> let newShip = damage shot_damage ship in unsafePerformIO $ do
                            print newShip
                            return $ case health newShip > 0 of
                                -- If it's still alive, just update it
                                True -> model {shots = Map.delete shotId shots, ships = Map.insert (getId ship) newShip ships}

                                -- Otherwise, get rid of it, because it's dead
                                False -> model {shots = Map.delete shotId shots, ships = Map.delete (getId ship) ships}
                        Nothing -> model

instance Physics Ship where
    getId = shipId
    getBounds (Ship {shipPos = V2 x y, shipSize=shipSize}) = Rect x y shipSize shipSize

    doMove ship@(Ship {..}) = ship {shipPos = shipPos + shipVel}
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

damage :: Int -> Ship -> Ship
damage amount ship@(Ship {..}) = ship {health = health - amount}

-- Shoots a shot from the specified ship at the specified position.
createShot :: Model -> Ship -> V2 Double -> Model
createShot model@(Model {..}) ship@(Ship {..}) pos = model {shots=newShots, nShots = nShots + 1}
    where newShots = Map.insert nShots (Shot nShots shipPos vel shipId) shots
            where vel = normalize (pos - shipPos) * shot_speed

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

