{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Main
    (
        GUIManager (..),
        GUIElement (..),
        handleClick,
        showGUI
    ) where

import Helm
import Helm.Color
import Helm.Graphics2D

import qualified Helm.Mouse as Mouse
import qualified Helm.Graphics2D.Text as Text
import qualified Helm.Keyboard as Keyboard

import qualified Data.Map as Map
import Data.Maybe (isJust)

import Linear.V2 (V2(V2))

import Misc

import Model.Action

data GUIManager = GUIManager { guiElements :: Map.Map String GUIElement }
    deriving Show

data GUIElement = Button { buttonId :: String,
                           buttonText :: String,
                           buttonPos :: V2 Double,
                           buttonSize :: V2 Double,
                           buttonColor :: Color,
                           textHeight :: Double,
                           textColor :: Color,
                           buttonAction :: V2 Double -> Action }

instance Show GUIElement where
    show Button{..} = buttonText

showGUI :: GUIManager -> Form e
showGUI guiManager@GUIManager{..} = group $ map showGUIElement $ Map.elems guiElements

showGUIElement :: GUIElement -> Form e
showGUIElement Button{..} =
    group $ [move buttonPos $ filled buttonColor $ rect buttonSize,
             move buttonPos $ text $ Text.typeface "Callibri" $ Text.height textHeight $ Text.color textColor $ Text.toText buttonText]

handleClick :: GUIManager -> Mouse.MouseButton -> V2 Double -> Maybe Action
handleClick manager@GUIManager{guiElements} button pos =
    case filter isJust $ map (handleGUIClick button pos) $ Map.elems guiElements of
        (a@(Just _):_) -> a
        _ -> Nothing

handleGUIClick :: Mouse.MouseButton -> V2 Double -> GUIElement -> Maybe Action
handleGUIClick button pos self@Button{buttonPos = V2 sx sy, buttonSize=V2 w h, buttonAction}
    | pos `inRect` bounds = Just $ buttonAction pos
    | otherwise = Nothing
    where bounds = Rect sx sy w h 0


setGUIId v self@Button{..} = self { buttonId = v}
getGUIId self@Button{..} = buttonId
