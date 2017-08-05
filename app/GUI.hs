module GUI
    (
        buildMainGUI
    ) where

import qualified Data.Map as Map

import Linear.V2 (V2(V2))

import Model.Action

import GUI.Main

import Ships

import Helm.Color

buildMainGUI :: GUIManager
buildMainGUI = GUIManager
               {
                    guiElements = guiElements,
                    nElements = Map.size guiElements
               }
    where guiElements =
            Map.fromList
            [
                (1, Button
                    {
                        buttonId = 1,
                        buttonText = "Add Ship",
                        buttonPos = V2 80 50,
                        buttonSize = V2 100 25,
                        buttonColor = rgb 1 0.941 0.627,
                        textColor = rgb 0 0 0,
                        buttonAction = const AddRandomShip
                    }
                )
            ]
