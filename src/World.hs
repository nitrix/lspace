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
    [ (coordinate 0 0, [boxObject defaultObject defaultBox])
    , (coordinate 1 0, [boxObject defaultObject defaultBox])
    , (coordinate 0 1, [boxObject defaultObject defaultBox])
    , (coordinate 2 1, [boxObject defaultObject $ defaultBox { boxState = BoxOpened }])
    , (coordinate 1 2, [boxObject defaultObject $ defaultBox { boxState = BoxClosedLocked }])
    , (coordinate 3 1, [boxObject defaultObject $ defaultBox { boxState = BoxOpened }])
    , (coordinate 0 0, [boxObject defaultObject $ defaultBox { boxState = BoxClosedLocked }])
    , (coordinate 1 3, [boxObject defaultObject defaultBox])
    , (coordinate 5 2, [boxObject defaultObject defaultBox])
    ]

worldObjectsAt :: World -> Coordinate -> [Object]
worldObjectsAt world coord = fromMaybe [] $ M.lookup coord world

-- TODO: temporary test
worldTestInteractAll :: World -> World
worldTestInteractAll w = map (\o -> snd $ objUpdate o InteractMsg) <$> w -- output msgs are discarded
