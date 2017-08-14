{-# LANGUAGE NamedFieldPuns #-}

module Ships
    (
        loadData,
        defaultLoadData
    ) where

import Control.Monad

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.String.Utils as StrUtils

import Linear.V2 (V2(V2))

import System.Random

import Helm.Color

import Model.Main
import qualified Model.Part as Part
import qualified Model.Ship as Ship

import Controller.Main
import qualified Controller.Part as Part

makePartDef :: Part.Part -> (String, Part.Part)
makePartDef part@Part.Part{Part.name} = (name, part)

type IntermediateItem = ((Direction, String), (String, String))
type IntermediatePattern = (Ship.Ship, [IntermediateItem])

defaultLoadData :: String -> IO [BuildPattern]
defaultLoadData path = do
    (_, patterns) <- loadData (Map.empty, []) path

    return patterns

loadData :: (Map.Map String Part.Part, [BuildPattern]) -> String -> IO (Map.Map String Part.Part, [BuildPattern])
loadData inData path = do
    putStrLn $ "Loading data from: " ++ path

    contents <- readFile "app/data.txt" >>= (return . filter (not . StrUtils.startswith "--") . lines)

    let dataFiles = filter (StrUtils.startswith "data") contents
    let parts = filter (StrUtils.startswith "part") contents
    let ships = filter (StrUtils.startswith "ship") contents

    putStrLn $ "Data files: " ++ show dataFiles
    putStrLn $ "Parts: " ++ show parts
    putStrLn $ "Ships: " ++ show ships

    -- First load everything from the data files referenced here.
    (newPartDefs, patterns) <- foldM loadData inData dataFiles

    -- Now load the stuff referenced in this file
    newParts <- mapM readFileContent $ map getAfterDelim parts

    let loadedPartDefs = Map.union newPartDefs $ Map.fromList $ map makePartDef newParts

    newShips <- mapM readFileContent $ map getAfterDelim ships

    -- Resolve the names of the parts the load into the actual part objects.
    let loadedShips = map (\(ship, shipParts) -> (ship, map (getPartReferences loadedPartDefs) shipParts)) newShips

    return (loadedPartDefs, loadedShips)

    where readFileContent path = readFile path >>= (return . read)
          getAfterDelim s = concat $ tail $ StrUtils.split ":" s
          getPartReferences partDefs ((dir, neighbor), (partName, newName)) =
            ((dir, neighbor), (fromJust $ Map.lookup partName partDefs, newName))

