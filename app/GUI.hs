{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module GUI
    (
        buildMainGUI,
        updateMainGUI
    ) where

import qualified Data.Map as Map
import Data.String.Utils (startswith)

import Linear.V2 (V2(V2))

import Model.Action
import Model.Main
import qualified Model.Part as Part
import qualified Model.Ship as Ship

import qualified Controller.Part as Part

import GUI.Main

import Ships

import Helm.Color

bg = rgb 1 (208/255) (68/255)
foregroundColor = rgb (20/255) (50/255) (90/255)

-- Updates the GUI (mostly does part stuff at the moment)
updateMainGUI :: Model -> Model
updateMainGUI model@Model{guiManager = guiManager@GUIManager{guiElements}} = 
    model {guiManager = newGuiManager}
    where newGuiManager = guiManager {guiElements = newGuiElements}
          newGuiElements = Map.union normalGUI $ createSelectedShipGUI model
          normalGUI = Map.filter (\g -> not $ startswith "#" $ guiId g) guiElements

createSelectedShipGUI :: Model -> Map.Map String GUIElement
createSelectedShipGUI model@Model{ships, currentShip} = 
    case Map.lookup currentShip ships of
        Nothing -> Map.empty
        Just ship -> createShipGUI model ship 

-- Create the GUI stuff for the ship that's selected.
-- Health bars, buttons to allow selecting parts, etc.
createShipGUI :: Model -> Ship.Ship -> Map.Map String GUIElement
createShipGUI model@Model{ships} ship =
    foldl Map.union Map.empty $ map createPartGUI $ Map.elems $ Part.getParts model ship

createPartGUI :: Part.Part -> Map.Map String GUIElement
createPartGUI Part.Part{..} =
    Map.fromList [nameLabel]

    where prefix = "#part-" ++ show id ++ "-"
          nameLabel = 
            (
                prefix ++ "name",
                GUIElement
                {
                    guiId = prefix ++ "name",
                    text = name,
                    pos = pos,
                    size = V2 50 20,
                    color = Color 1 1 1 1,
                    backgroundColor = Color 0 0 0 0,
                    textHeight = 12,
                    specs = Label
                }
            )

buildMainGUI :: GUIManager
buildMainGUI = GUIManager
               {
                    guiElements = guiElements,
                    focus = Nothing
               }
    where guiElements =
            Map.fromList
            [
                ("addButton", 
                 GUIElement
                 {
                     guiId = "addButton",
                     text = "Add Ship",
                     pos = V2 100 50,
                     size = V2 150 60,
                     color = foregroundColor,
                     backgroundColor = bg, 
                     textHeight = 20,
                     specs = Button
                             {
                                buttonAction = const AddRandomShip
                             }
                 }
                ),
                ("reloadButton",
                 GUIElement
                 {
                    guiId = "reloadButton",
                    text = "Reload",
                    pos = V2 100 120,
                    size = V2 150 60,
                    color = foregroundColor,
                    backgroundColor = bg,
                    textHeight = 20,
                    specs = Button
                            {
                                buttonAction = const ReloadPatterns
                            }
                 }
                )
            ]

