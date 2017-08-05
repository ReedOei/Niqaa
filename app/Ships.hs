{-# LANGUAGE NamedFieldPuns #-}

module Ships
    (
        loadPatterns
    ) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)

import Linear.V2 (V2(V2))

import System.Random

import Helm.Color

import Model.Main
import qualified Model.Part as Part
import qualified Model.Ship as Ship

import Controller.Main
import qualified Controller.Part as Part

partDefinitions :: Map.Map String Part.Part
partDefinitions = Map.fromList
                  [
                      ("base", base),
                      ("miniBase", miniBase),
                      ("mediBase", mediBase),
                      ("gun", gun),
                      ("gatlingGun", gatlingGun),
                      ("howitzer", howitzer),
                      ("broadSide", broadSide),
                      ("machineGun", machineGun),
                      ("megaLaser", megaLaser),
                      ("bigBullet", bigBullet),
                      ("slingShot", slingShot)
                  ]

type IntermediateItem = ((Direction, String), (String, String))
type IntermediatePattern = (Ship.Ship, [IntermediateItem])

loadPatterns :: IO [BuildPattern]
loadPatterns = do
    contents <- readFile "app/Ships.txt"
    let (parts, intermediatePatterns) = read contents :: ([Part.Part], [IntermediatePattern])
    let newParts = Map.fromList $ map (\part@Part.Part{Part.name} -> (name, part)) parts
    let partDefs = Map.union newParts partDefinitions

    return $ map (\(ship, items) -> (ship, map (go partDefs) items)) intermediatePatterns
    where go partDefs ((dir, neighbor), (partName, newName)) = ((dir, neighbor), (fromJust $ Map.lookup partName partDefs, newName))
    
base :: Part.Part
base = Part.Part
    {
        Part.id = -1,
        Part.name = "",
        Part.shipId = -1,
        Part.pos = V2 0 0,
        Part.vel = V2 0 0,
        Part.health = 50,
        Part.color = rgb 1 1 1,
        Part.size = 20,
        Part.factionId = -1,
        Part.stats = Part.Hull
    }

miniBase = Part.Part
    {
        Part.id = -1,
        Part.name = "",
        Part.shipId = -1,
        Part.pos = V2 0 0,
        Part.vel = V2 0 0,
        Part.health = 15,
        Part.color = rgb 1 1 1,
        Part.size = 3,
        Part.factionId = -1,
        Part.stats = Part.Hull
    }

mediBase = Part.Part
    {
        Part.id = -1,
        Part.name = "",
        Part.shipId = -1,
        Part.pos = V2 0 0,
        Part.vel = V2 0 0,
        Part.health = 20,
        Part.color = rgb 1 1 1,
        Part.size = 8,
        Part.factionId = -1,
        Part.stats = Part.Hull
    }

gun :: Part.Part
gun = Part.makeGun 5 5 $ Part.Gun
    {
        Part.prec = 5,
        Part.timerGoal = 2000,
        Part.timer = 0,
        Part.shotSize = 3,
        Part.shotDamage = 3,
        Part.shotSpeed = 1,
        Part.salvoSize = 0,
        Part.shotsLeft = 0,
        Part.salvoTimerGoal = 0,
        Part.salvoTimer = 0
    }

gatlingGun = Part.makeGun 30 15 $ Part.Gun
    {
        Part.prec = 6,
        Part.timerGoal = 150,
        Part.timer = 0,
        Part.shotSize = 2,
        Part.shotDamage = 1,
        Part.shotSpeed = 1.5,
        Part.salvoSize = 0,
        Part.shotsLeft = 0,
        Part.salvoTimerGoal = 0,
        Part.salvoTimer = 0
    }

howitzer = Part.makeGun 35 12 $ Part.Gun
    {
        Part.prec = 2,
        Part.timerGoal = 8000,
        Part.timer = 0,
        Part.shotSize = 6,
        Part.shotDamage = 30,
        Part.shotSpeed = 3,
        Part.salvoSize = 0,
        Part.shotsLeft = 0,
        Part.salvoTimerGoal = 0,
        Part.salvoTimer = 0
    }

broadSide = Part.makeGun 20 15 $ Part.Gun
    {
        Part.prec = 5,
        Part.timerGoal = 10000,
        Part.timer = 0,
        Part.shotSize = 4,
        Part.shotDamage = 6,
        Part.shotSpeed = 1.5,
        Part.salvoSize = 5,
        Part.shotsLeft = 0,
        Part.salvoTimerGoal = 100,
        Part.salvoTimer = 0
    }

machineGun = Part.makeGun 35 40 $ Part.Gun
    {
        Part.prec = 10,     -- I imagine machine guns are not that accurate
        Part.timerGoal = 35000,
        Part.timer = 17500,
        Part.shotSize = 2,   -- Shots are probably pretty small?
        Part.shotDamage = 4,
        Part.shotSpeed = 1.5,
        Part.salvoSize = 200,
        Part.shotsLeft = 0,
        Part.salvoTimerGoal = 0,
        Part.salvoTimer = 0
    }

-- This gun is very technologically advanced: it fires very precise shots, 2 shots at a time, in rapid succession. Yet the shots are very weak and small, and the gun itself is rather fragile. A small amount of damage is devastating to the gun.
megaLaser = Part.makeGun 3 30 $ Part.Gun
    {
        Part.prec = 1,
        Part.timerGoal = 50,
        Part.timer = 0,
        Part.shotSize = 1,
        Part.shotDamage = 0.25,
        Part.shotSpeed = 5,
        Part.salvoSize = 1,
        Part.shotsLeft = 0,
        Part.salvoTimerGoal = 0,
        Part.salvoTimer = 0
    }

-- This is a very massive gun developed by the Big Gun People. It's a very "manly man's" kind of gun. Very showy. Very inaccurate. But devastatingly powerful.
bigBullet = Part.makeGun 50 50 $ Part.Gun
    {
        Part.prec = 30,
        Part.timerGoal = 10000,
        Part.timer = 0,
        Part.shotSize = 7,
        Part.shotDamage = 100,
        Part.shotSpeed = 1,
        Part.salvoSize = 1,
        Part.shotsLeft = 0,
        Part.salvoTimerGoal = 0,
        Part.salvoTimer = 0
    }

-- This is more of your "this is our first try at the whole space travel thing. we're, like, prolly the only people advanced enough to make it into space, so these guns are probably good enough. we'll prolly only need them against, like, spacewhales" kind of gun.
slingShot = Part.makeGun 1 2 $ Part.Gun
    {
        Part.prec = 25,
        Part.timerGoal = 3000,
        Part.timer = 0,
        Part.shotSize = 1,
        Part.shotDamage = 0.5,
        Part.shotSpeed = 0.5,
        Part.salvoSize = 1,
        Part.shotsLeft = 0,
        Part.salvoTimerGoal = 0,
        Part.salvoTimer = 0
    }


