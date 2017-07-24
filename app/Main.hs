{-# LANGUAGE RecordWildCards #-}

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

-- All of the types and actual game functions are defined in here/sub libraries.
import Lib

initial :: (Model, Cmd SDLEngine Action)
initial = (Model 1 initShips Map.empty (Map.size initShips + 1) 1, Cmd.none)
    where initShips = Map.fromList [(1, Ship 1 "Kiraara" 40 (V2 500 500) (V2 0 0) 10),
                                    (2, Ship 2 "Vijossk" 30 (V2 600 100) (V2 0 0) 10)]

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
-- Change the current ship when we right click
update model@(Model {..}) (RClick pos) =
    case findShipAt pos ships of
        Just shipName -> (model {currentShip = shipName}, Cmd.none)
        Nothing -> (model {currentShip = (-1)}, Cmd.none)

-- Shoot a shot from our ship if we can at the place we clicked on
update model@(Model {..}) (LClick pos) = 
    case getCurrentShip model of
        Just ship -> (createShot model ship pos, Cmd.none)
        Nothing -> (model, Cmd.none)

update model (Step dt) = (handlePhysics model, Cmd.none)
update model None = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [Mouse.clicks handleClick,
                           Time.fps 60 Step]
    where handleClick Mouse.LeftButton (V2 x y) = LClick (V2 (fromIntegral x) (fromIntegral y))
          handleClick Mouse.RightButton (V2 x y) = RClick (V2 (fromIntegral x) (fromIntegral y))
          handleClick _ _ = None

view :: Model -> Graphics SDLEngine
view (Model {..}) = Graphics2D $ collage (map showShip (Map.elems ships) ++ map showShot (Map.elems shots))
    where showShip (Ship {..}) = group [move shipPos $ filled (rgb 1 0 0) $ square shipSize,
                                        -- The (-10) adjustment is because the text is 12 pixels tall, so we add 7 to get it out of the ship
                                        -- and another few so it's not directly on it.
                                        move (V2 x (y - shipSize / 2 - 10)) $ text $ Text.height 12 $ Text.color (rgb 1 0 0) $ Text.toText shipName]
            where (V2 x y) = shipPos
          showShot (Shot {..}) = move shotPos $ filled (rgb 0 0 1) $ square shot_size

main :: IO ()
main = do
    engine <- SDL.startup

    run engine GameConfig
     {
        initialFn = initial,
        updateFn = update,
        subscriptionsFn = subscriptions,
        viewFn = view
     }
