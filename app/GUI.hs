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

backgroundColor = rgb 1 (208/255) (68/255)
foregroundColor = rgb (20/255) (50/255) (90/255)

buildMainGUI :: GUIManager
buildMainGUI = GUIManager
               {
                    guiElements = guiElements
               }
    where guiElements =
            Map.fromList
            [
                ("addButton", 
                 Button
                 {
                     buttonId = "addButton",
                     buttonText = "Add Ship",
                     buttonPos = V2 100 50,
                     buttonSize = V2 150 60,
                     textHeight = 20,
                     buttonColor = backgroundColor, 
                     textColor = foregroundColor,
                     buttonAction = const AddRandomShip
                 }
                ),
                ("reloadButton",
                 Button
                 {
                    buttonId = "reloadButton",
                    buttonText = "Reload",
                    buttonPos = V2 100 120,
                    buttonSize = V2 150 60,
                    textHeight = 20,
                    buttonColor = backgroundColor,
                    textColor = foregroundColor,
                    buttonAction = const ReloadPatterns
                 }
                )
            ]

