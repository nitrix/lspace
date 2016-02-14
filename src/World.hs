module World where

import Data.Maybe
import qualified Data.Map as M
import Object
import Coordinate

import Object.Door

type World = M.Map Coordinate [Object]

defaultWorld :: World
defaultWorld = M.fromList [(coordinate 0 0, [FloorObject])
                          ,(coordinate 1 0, [WallObject])
                          ,(coordinate 2 0, [WallObject])
                          ,(coordinate 1 1, [WallObject])
                          ,(coordinate 2 1, [WallObject])
                          ,(coordinate 3 1, [WallObject])
                          ,(coordinate 0 2, [DoorObject DoorStateOpened])
                          ,(coordinate 1 2, [DoorObject DoorStateClosed])
                          ]

worldObjectsAt :: World -> Coordinate -> [Object]
worldObjectsAt world coord = fromMaybe [] $ M.lookup coord world
