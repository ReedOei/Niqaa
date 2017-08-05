module Controller.Main
    (
        Physics (..),
        v2Len, normalize, average,
        inRect,
        updatePhysics,
        getDirection, opposite
    ) where

import qualified Data.Map as Map

import Linear.V2 (V2(V2))

import Model.Main

import Misc

average :: Floating a => [a] -> a
average xs = sum xs / (fromIntegral $ length xs)

setAt :: [a] -> Int -> a -> [a]
setAt xs i e = take i xs ++ [e] ++ drop (i + 1) xs

v2Len :: Floating a => V2 a -> a
v2Len (V2 x y) = sqrt $ x^2 + y^2

normalize :: Floating a => V2 a -> V2 a
normalize v = v / (pure $ v2Len v)

collide :: Rect -> Rect -> Bool
collide (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
    x1 - w1 / 2 <= x2 + w2 / 2 &&
    x1 + w1 / 2 >= x2 - w2 / 2 &&
    y1 - h1 / 2 <= y2 + h2 / 2 &&
    y1 + h1 / 2 >= y2 - h2 / 2

opposite U = D
opposite D = U
opposite L = R
opposite R = L

-- Checks which direction the second point is relative to the first
getDirection :: Ord a => V2 a -> V2 a -> Maybe Direction
getDirection (V2 x1 y1) (V2 x2 y2)
    | x1 < x2 = Just R
    | x1 > x2 = Just L
    | y1 < y2 = Just D
    | y1 > y2 = Just U
    | otherwise = Nothing

class Physics o where
    getId :: o -> Int

    doMove :: o -> o
    handleMove :: Model -> o -> Model

    getBounds :: o -> Rect

    handleCollisions :: Model -> o -> Model

    handleStep :: Double -> Model -> o -> Model

    checkCollisions :: Physics a => o -> Map.Map k a -> [a]
    checkCollisions self m = filter (collided self) $ Map.elems m

    collided :: Physics a => o -> a -> Bool
    collided a b = collide (getBounds a) (getBounds b)

    doPhysics :: Model -> o -> Model
    doPhysics model self = handleCollisions (handleMove model self) (doMove self)

updatePhysics :: Physics a => Map.Map Int a -> a -> Map.Map Int a
updatePhysics m self = Map.insert (getId self) self m
