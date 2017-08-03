module Ships
    (
        kiraara,
        vijossk,
        videre,
        hija,
        davanja,
        pischki,
        allShips
    ) where

import Linear.V2 (V2(V2))

import System.Random

import Model.Main
import qualified Model.Part as Part
import qualified Model.Ship as Ship

import Controller.Main
import qualified Controller.Part as Part

randRange :: (Random a, Num a) => (a, a) -> IO a
randRange r = getStdRandom (randomR r)

allShips = [pischki, kiraara, vijossk, videre, hija, davanja]

base :: Part.Part
base = Part.Part 
    {
        Part.id = -1,
        Part.name = "",
        Part.shipId = -1,
        Part.pos = V2 0 0,
        Part.vel = V2 0 0,
        Part.health = 50,
        Part.color = (1,1,1),
        Part.size = 20,
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
        Part.timer = 0,
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

-- This is the byproduct of a very whimsical gunmaker.
{-explosiveFrisbee = Part.makeGun 15 20 $ Part.Gun
    {
        Part.prec = floor $ rand (1,20),
        Part.timerGoal = floor $ rand (1000, 10000),
        Part.timer = 0,
        Part.shotSize = 1.3,   -- per official explosive frisbee standards
        Part.shotDamage = rand (2, 2.2),
        Part.shotSpeed = 1.5,
        Part.salvoSize = floor $ rand (1,3), -- one machine can only throw
        Part.shotsLeft = 0,
        Part.salvoTimerGoal = floor $ rand (20, 40),
        Part.salvoTimer = 0
    }
          where rand range = do
                  random <- randRange range
                  return random-}

pischki =
    (
        Ship.Ship
        {
            Ship.id = -1,
            Ship.name = "Pischki",
            Ship.classType = "Cruiser",
            Ship.classAbb = "CA",
            Ship.color = (1, 0.5, 0),
            Ship.factionId = 6,
            Ship.pos = V2 700 400
        },
        [
            ((U, "ship"), (base, "ship")),
            ((L, "ship"), (base, "left1")),
            ((L, "left1"), (base, "left2")),
            ((R, "ship"), (base, "right1")),
            ((R, "right1"), (base, "right2")),
            ((U, "ship"), (base, "top")),
            ((D, "ship"), (base, "bottom")),
            ((L, "left2"), (gun, "leftPD")),
            ((R, "right2"), (gun, "rightPD")),
            ((U, "top"), (broadSide, "topBroadside")),
            ((D, "bottom"), (broadSide, "bottomBroadside")),
            ((L, "top"), (broadSide, "leftBroadside1")),
            ((L, "bottom"), (broadSide, "leftBroadside2")),
            ((R, "top"), (broadSide, "rightBroaside1")),
            ((R, "bottom"), (broadSide, "rightBroaside2"))
        ]
    )

kiraara = 
    (
        Ship.Ship
        {
            Ship.id = -1,
            Ship.name = "Kiraara",
            Ship.classType = "Destroyer",
            Ship.classAbb = "DD",
            Ship.color = (1,0,0),
            Ship.factionId = 1,
            Ship.pos = (V2 1500 200)
        },
        [
            ((U, "ship"), (base, "ship")),
            ((U, "ship"), (base, "top")),
            ((D, "ship"), (base, "bottom")),
            ((U, "top"), (gatlingGun, "gatlingGun")),
            ((D, "bottom"), (gun, "tailGun")),
            ((L, "ship"), (gun, "leftGun")),
            ((R, "ship"), (gun, "rightGun"))
        ]
    )

vijossk =
    (
        Ship.Ship
        {
            Ship.id = -1,
            Ship.name = "Vijossk",
            Ship.classType = "Freighter",
            Ship.classAbb = "FT",
            Ship.color = (0,0,1),
            Ship.factionId = 2,
            Ship.pos = V2 600 100
        },
        [
            ((U, "ship"), (base, "ship")),
            ((U, "ship"), (base, "top")),
            ((D, "ship"), (base, "bottom")),
            ((D, "bottom"), (gun, "pointGun"))
        ]
    )

videre = 
    (
        Ship.Ship 
        {
            Ship.id = -1,
            Ship.name = "Videre",
            Ship.classType = "Escort",
            Ship.classAbb = "ECT",
            Ship.color = (0,1,0),
            Ship.factionId = 3,
            Ship.pos = V2 500 600
        },
        [
            ((U, "ship"), (base, "ship")),
            ((R, "ship"), (base, "starboard")),
            ((L, "ship"), (base, "port")),
            ((R, "starboard"), (gun, "rightGun")),
            ((L, "port"), (gun, "leftGun"))
        ]
    )

hija =
    (
        Ship.Ship
        {
            Ship.id = -1,
            Ship.name = "Hija",
            Ship.classType = "Interceptor",
            Ship.classAbb = "INT",
            Ship.color = (1,0,1),
            Ship.factionId = 4,
            Ship.pos = V2 100 500
        },
        [
            ((U, "ship"), (base, "ship")),
            ((L, "ship"), (gun, "leftGun")),
            ((R, "ship"), (gun, "rightGun")),
            ((U, "ship"), (gun, "topGun"))
        ]
    )

davanja =
    (
        Ship.Ship
        {
            Ship.id = -1,
            Ship.name = "Davanja",
            Ship.classType = "Artillery",
            Ship.classAbb = "ART",
            Ship.color = (0,1,1),
            Ship.factionId = 5,
            Ship.pos = V2 1550 700
        },
        [
            ((U, "ship"), (base, "ship")),
            ((U, "ship"), (howitzer, "howitzer"))
        ]
    )
