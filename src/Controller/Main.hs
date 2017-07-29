module Controller.Main
    (
        Physics (..),
        v2Len, normalize, average,
        inRect
    ) where

import qualified Data.Map as Map

import Linear.V2 (V2(V2))

import Model.Main

average :: Floating a => [a] -> a
average xs = sum xs / (fromIntegral $ length xs)

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


