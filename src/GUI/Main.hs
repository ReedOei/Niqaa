{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Main
    (
        GUIManager (..),
        GUIElement (..),
        GUISpec (..),
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

data GUIManager = GUIManager { guiElements :: Map.Map String GUIElement,
                               focus :: Maybe String -- The element which is currently in focus. 
                               }
    deriving Show

data GUIElement = GUIElement { guiId :: String,
                               text :: String,
                               pos :: V2 Double,
                               size :: V2 Double,
                               color :: Color,
                               backgroundColor :: Color,
                               textHeight :: Double,
                               specs :: GUISpec }
                               
data GUISpec = Button { buttonAction :: V2 Double -> Action } |
               Label |
               Entry

instance Show GUIElement where
    show GUIElement{..} = text

showGUI :: GUIManager -> Form e
showGUI guiManager@GUIManager{..} = group $ map showGUIElement $ Map.elems guiElements

showGUIElement :: GUIElement -> Form e
showGUIElement GUIElement{..} =
    group $ [move pos $ filled backgroundColor $ rect size,
             move pos $ Helm.Graphics2D.text $ Text.typeface "Callibri" $ Text.height textHeight $ Text.color color $ Text.toText text]

handleClick :: GUIManager -> Mouse.MouseButton -> V2 Double -> Maybe Action
handleClick manager@GUIManager{guiElements} button pos =
    case filter isJust $ map (handleGUIClick button pos) $ Map.elems guiElements of
        (a@(Just _):_) -> a
        _ -> Nothing

handleGUIClick :: Mouse.MouseButton -> V2 Double -> GUIElement -> Maybe Action
handleGUIClick button pos self@GUIElement{pos = V2 sx sy, size=V2 w h, specs}
    | pos `inRect` bounds = 
        case specs of
            Button{..} -> Just $ buttonAction pos
            _ -> Nothing
    | otherwise = Nothing
    where bounds = Rect sx sy w h 0

setId v self@GUIElement{..} = self { guiId = v}
getId self@GUIElement{..} = guiId

