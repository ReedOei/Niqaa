module Ships
    (
        kiraara,
        vijossk,
        videre,
        hija,
        davanja,
        pischki,
        jin,
        tiktok,
        allShips
    ) where

import qualified Data.Map as Map

import Linear.V2 (V2(V2))

import System.Random

import Helm.Color

import Model.Main
import qualified Model.Part as Part
import qualified Model.Ship as Ship

import Controller.Main
import qualified Controller.Part as Part

partDefinitions :: Map.Map String Part.Part
partDefinitions = Map.empty

allShips = [pischki, kiraara, vijossk, videre, hija, davanja, jin, tiktok, blaqiiiiip]
--allShips = [tiktok, tiktok, pischki, blaqiiiiip, blaqiiiiip, blaqiiiiip, blaqiiiiip, blaqiiiiip]

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

pischki =
    (
        Ship.Ship
        {
            Ship.id = -1,
            Ship.name = "Pischki",
            Ship.classType = "Cruiser",
            Ship.classAbb = "CA",
            Ship.color = rgb 1 0.5 0,
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
            Ship.color = rgb 1 0 0,
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
            Ship.color = rgb 0 0 1,
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
            Ship.color = rgb 0 1 0,
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
            Ship.color = rgb 1 0 1,
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
            Ship.color = rgb 0 1 1,
            Ship.factionId = 5,
            Ship.pos = V2 1550 700
        },
        [
            ((U, "ship"), (base, "ship")),
            ((U, "ship"), (howitzer, "howitzer"))
        ]
    )

jin =
    (
        Ship.Ship
        {
            Ship.id = -1,
            Ship.name = "Jin",
            Ship.classType = "Fighter",
            Ship.classAbb = "FTR",
            Ship.color = rgb 0.5 0.5 0,
            Ship.factionId = 7,
            Ship.pos = V2 0 0
        },
        [
            ((U, "ship"), (base, "ship")),
            ((R, "ship"), (megaLaser, "megaLaser")),
            ((L, "ship"), (megaLaser, "megaLaser2"))
        ]
    )

tiktok =
    (
        Ship.Ship
        {
            Ship.id = -1,
            Ship.name = "Tiktok",
            Ship.classType = "Artillery",
            Ship.classAbb = "ART",
            Ship.color = rgb 0.8627 0.078431 0.23529,
            Ship.factionId = 8,
            Ship.pos = V2 0 0
        },
        [
            ((U, "ship"), (base, "ship")),
            ((R, "ship"), (miniBase, "rightBow")),
            ((R, "rightBow"), (megaLaser, "megaLaser")),
            ((U, "megaLaser"), (megaLaser, "megaLaser2")),
            ((U, "megaLaser2"), (megaLaser, "megaLaser3")),
            ((U, "megaLaser3"), (megaLaser, "megaLaser4")),
            ((L, "ship"), (miniBase, "leftBow")),
            ((L, "leftBow"), (megaLaser, "megaLaser5")),
            ((U, "megaLaser5"), (megaLaser, "megaLaser6")),
            ((U, "megaLaser6"), (megaLaser, "megaLaser7")),
            ((U, "megaLaser7"), (megaLaser, "megaLaser8"))
        ]
    )


blaqiiiiip =
    (
        Ship.Ship
        {
            Ship.id = -1,
            Ship.name = "Blaqiiiiip",
            Ship.classType = "Destroyer",
            Ship.classAbb = "DD",
            Ship.color = rgb 1 1 0.4,
            Ship.factionId = 9,
            Ship.pos = V2 0 0
        },
        [
            ((U, "ship"), (base, "ship")),

            ((R, "ship"), (mediBase, "mediBaseR1")),
            ((R, "mediBaseR1"), (mediBase, "mediBaseR2")),
            ((R, "mediBaseR2"), (mediBase, "mediBaseR3")),
            ((R, "mediBaseR3"), (mediBase, "mediBaseR4")),
            ((R, "mediBaseR4"), (mediBase, "mediBaseR5")),
            ((R, "mediBaseR5"), (mediBase, "mediBaseR6")),
            ((U, "mediBaseR6"), (mediBase, "mediBaseRU")),
            ((U, "mediBaseRU"), (machineGun, "machineGunR1")),
            ((D, "mediBaseR6"), (mediBase, "mediBaseRD")),
            ((D, "mediBaseRD"), (machineGun, "machineGunR2")),

            ((L, "ship"), (mediBase, "mediBaseL1")),
            ((L, "mediBaseL1"), (mediBase, "mediBaseL2")),
            ((L, "mediBaseL2"), (mediBase, "mediBaseL3")),
            ((L, "mediBaseL3"), (mediBase, "mediBaseL4")),
            ((L, "mediBaseL4"), (mediBase, "mediBaseL5")),
            ((L, "mediBaseL5"), (mediBase, "mediBaseL6")),
            ((U, "mediBaseL6"), (mediBase, "mediBaseLU")),
            ((U, "mediBaseLU"), (machineGun, "machineGunL1")),
            ((D, "mediBaseL6"), (mediBase, "mediBaseLD")),
            ((D, "mediBaseLD"), (machineGun, "machineGunL2"))

        ]
    )

