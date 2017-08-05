{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Main
    (
        GUIManager (..),
        GUIElement (..)
    ) where
    
import qualified Data.Map as Map
import Data.Maybe (isJust)

import Linear.V2 (V2(V2))
    
import Helm
import qualified Helm.Mouse as Mouse

import Misc

import Model.Action

data GUIManager = GUIManager { guiElements :: Map.Map Int GUIElement, nElements :: Int }
    deriving Show

data GUIElement = Button { buttonId :: Int,
                           buttonText :: String,
                           buttonPos :: V2 Double,
                           buttonSize :: V2 Double,
                           buttonAction :: V2 Double -> Action }

instance Show GUIElement where
    show Button{..} = buttonText

handleLClick :: GUIManager -> V2 Double -> Maybe Action
handleLClick manager@GUIManager{guiElements} pos = 
    case filter isJust $ map (handleClick Mouse.LeftButton pos) $ Map.elems guiElements of
        (a@(Just _):_) -> a
        _ -> Nothing

handleClick button pos self@Button{buttonPos = V2 sx sy, buttonSize=V2 w h, buttonAction}
    | pos `inRect` bounds = Just $ buttonAction pos 
    | otherwise = Nothing
    where bounds = Rect sx sy w h


setGUIId v self@Button{..} = self { buttonId = v}
getGUIId self@Button{..} = buttonId 

addGUIElement manager@GUIManager{..} self = 
    manager { guiElements = Map.insert nElements (setGUIId nElements self) guiElements, 
              nElements = nElements + 1}
