module World where

import Coordinate
import qualified Data.Map as M
import Data.Maybe

import Message
import Object
import Object.Box

type World = M.Map Coordinate [Object]

defaultWorld :: World
defaultWorld = M.fromList
    [ (coordinate 0 0, [boxObject defaultBox])
    , (coordinate 1 0, [boxObject defaultBox])
    , (coordinate 0 1, [boxObject $ defaultBox { boxState = BoxOpened }])
    , (coordinate 2 1, [boxObject $ defaultBox { boxState = BoxOpened }])
    , (coordinate 1 2, [boxObject $ defaultBox { boxState = BoxClosedLocked }])
    , (coordinate 3 1, [boxObject $ defaultBox { boxState = BoxOpened }])
    , (coordinate 0 0, [boxObject $ defaultBox { boxState = BoxClosedLocked }])
    , (coordinate 1 3, [boxObject defaultBox])
    , (coordinate 5 2, [boxObject defaultBox])
    ]

worldObjectsAt :: World -> Coordinate -> [Object]
worldObjectsAt world coord = fromMaybe [] $ M.lookup coord world

-- TODO: temporary test
worldTestInteractAll :: World -> World
worldTestInteractAll w = map (flip objUpdate InteractMsg) <$> w
