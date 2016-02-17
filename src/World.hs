module World where

import Coordinate
import qualified Data.Map as M
import Data.Maybe

import Message
import Object
import Object.Door

type World = M.Map Coordinate [Object]

defaultWorld :: World
defaultWorld = M.fromList
    [ (coordinate 0 0, [doorObject defaultDoor])
    , (coordinate 1 0, [])
    , (coordinate 2 0, [])
    , (coordinate 1 1, [])
    , (coordinate 2 1, [])
    , (coordinate 3 1, [])
    , (coordinate 0 2, [])
    , (coordinate 1 2, [])
    ]

worldObjectsAt :: World -> Coordinate -> [Object]
worldObjectsAt world coord = fromMaybe [] $ M.lookup coord world

-- TODO: temporary test
worldTestInteractAll :: World -> World
worldTestInteractAll w = map go <$> w
    where
        go o = objUpdate o InteractMsg
