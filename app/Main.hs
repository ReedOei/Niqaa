{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Linear.V2 (V2(V2))

import Helm
import Helm.Color
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

import qualified Helm.Cmd as Cmd
import qualified Helm.Mouse as Mouse
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Graphics2D.Text as Text
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Sub as Sub
import qualified Helm.Time as Time

import qualified Data.Map as Map

import System.Random
import System.IO.Unsafe

-- All of the types and actual game functions are defined in here/sub libraries.
import Lib

import Model.Main
import qualified Model.Part as Part
import qualified Model.Ship as Ship
import qualified Model.Shot as Shot

import Controller.Main
import qualified Controller.Part as Part
import qualified Controller.Ship as Ship
import qualified Controller.Shot as Shot

import GUI.Main

import Ships -- Contains ships definitions
import GUI -- Contains GUI definitions

initial :: (Model, Cmd SDLEngine Action)
initial = (model, Cmd.execute (getStdRandom random >>= (return . mkStdGen)) InitRandom) -- Command is to set up the initial random generator.
    where model = initModel
          initModel =
             Model
             {
                currentShip = -1,
                ships = Map.empty,
                parts = Map.empty,
                shots = Map.empty,
                nShips = 1,
                nShots = 1,
                nParts = 1,
                gen = mkStdGen 1,
                shipPatterns = [],
                worldSize = V2 world_width world_height,
                guiManager = buildMainGUI
             }

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model (InitRandom gen) = update (model {gen = gen}) $ LoadPatterns "apps/data.txt" 
update model ReloadPatterns = update model $ LoadPatterns "apps/data.txt"
update model (LoadPatterns path) = (model, Cmd.execute (defaultLoadData path) LoadedPatterns)
update model (LoadedPatterns patterns) = (model {shipPatterns = patterns}, Cmd.none)

-- Change the current ship when we right click
update model@(Model {..}) (RClick pos) =
    case Ship.findAt pos ships of
        Just shipId -> (model {currentShip = shipId}, Cmd.none)
        Nothing -> (model {currentShip = (-1)}, Cmd.none)

-- Shoot a shot from our ship if we can at the place we clicked on
update model@(Model {..}) (LClick pos) = 
    case handleClick guiManager Mouse.LeftButton pos of
        Just action -> update model action
        Nothing -> (model, Cmd.none)

update model (Step milliseconds) = (doStep dt $ checkDestroyed $ handlePhysics dt model, Cmd.none)
    where dt = milliseconds / 1000

update model None = (model, Cmd.none)
update model@Model{shipPatterns} AddRandomShip = update (model {gen = newGen}) (AddShip (V2 x y) (shipPatterns !! i))
    where (i, newGen1) = randomR (0, length shipPatterns - 1) $ gen model
          (x, newGen2) = randomR (0, world_width) newGen1
          (y, newGen) = randomR (0, world_height) newGen2

update model@Model{shipPatterns} (AddRandomShipPos pos) = update (model {gen = newGen}) (AddShip pos (shipPatterns !! i))
    where (i, newGen) = randomR (0, length shipPatterns - 1) $ gen model

update model (AddShip pos (ship, pattern)) = (Part.buildShip model (ship {Ship.pos = pos}, pattern), Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [Mouse.clicks handleClick,
                           Time.fps gameFPS Step]
    where handleClick Mouse.LeftButton (V2 x y) = LClick (V2 (fromIntegral x) (fromIntegral y))
          handleClick Mouse.RightButton (V2 x y) = RClick (V2 (fromIntegral x) (fromIntegral y))
          handleClick _ _ = None

view :: Model -> Graphics SDLEngine
view model@(Model {..}) = Graphics2D $ collage (map showShip (Map.elems ships) ++ map showShot (Map.elems shots) ++ [showGUI guiManager])
    where showShip ship@Ship.Ship{Ship.color, Ship.pos=V2 shipX _} =
                group $ (map showPart $ Map.elems $ Part.getParts model ship) ++ [name]
                -- we want it to be just slightly above the highest piece.
                where name = case Part.getFarthest U model ship of
                                Just part@Part.Part{Part.pos = V2 _ partY} ->
                                    move (V2 shipX (partY - Part.size part / 2 - 10)) $ text $ Text.height 12 $ Text.color color $ Text.toText $ Ship.name ship
                                Nothing -> text $ Text.toText ""
          showPart (Part.Part {..}) = 
            case stats of
                Part.Shield{..} -> 
                    if strength > 1 then 
                        group [move pos $ filled color $ square size, 
                               move pos $ filled shieldColor $ circle (shieldSize * strength / maxStrength)]
                    else move pos $ filled color $ square size
                _ -> move pos $ filled color $ square size

            where (Color r g b a) = color
                  shieldColor = Color r g b (a / 3)
          showShot (Shot.Shot {..}) = move pos $ filled shotColor $ square size

main :: IO ()
main = do
    engine <- SDL.startupWith $ SDL.defaultConfig
                { SDL.windowIsResizable = False,
                  SDL.windowDimensions = floor <$> (worldSize $ fst initial) }

    run engine GameConfig
     {
        initialFn = initial,
        updateFn = update,
        subscriptionsFn = subscriptions,
        viewFn = view
     }

