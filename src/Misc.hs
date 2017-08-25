module Misc
    (
        randomRn,
        distance, distanceSquared, angle, fromAngle,
        getX, getY, getPos,
        inRect,
        clamp,
        roundPrec
    ) where

import Linear.V2 (V2(V2))

import System.Random

import Model.Action

roundPrec digits v = (fromInteger $ round $ v * 10^digits) / (10^^digits)

clamp v low high
    | v > high = high
    | v < low = high
    | otherwise = v

getPos :: V2 a -> (a, a)
getPos (V2 x y) = (x, y)

getX = fst . getPos
getY = snd . getPos

distanceSquared :: Floating a => V2 a -> a
distanceSquared (V2 x y) = x^2 + y^2

distance :: Floating a => V2 a -> a
distance = sqrt . distanceSquared

angle :: (RealFloat a, Floating a) => V2 a -> a
angle (V2 x y) = atan2 y x * 180 / pi

fromAngle :: Floating a => a -> V2 a
fromAngle inA = V2 (cos a) (sin a)
    where a = inA / 180 * pi

randomRn :: (RandomGen g, Random a, Num n, Ord n) => (a, a) -> g -> n -> ([a], g)
randomRn range initGen count = randomRn [] initGen 0
    where randomRn cur gen i
            | i < count = randomRn (new:cur) newGen $ i + 1
            | otherwise = (cur, gen)
            where (new, newGen) = randomR range gen

-- x and y are the position of the center
inRect :: V2 Double -> Shape -> Bool
inRect (V2 x1 y1) (Rect x2 y2 w h _) = x1 `inRange` (x2 - w, x2 + w) && y1 `inRange` (y2 - h, y2 + h)
    where inRange v (l, h) = v >= l && v <= h

