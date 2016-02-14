module Object where

import Linear (V2(V2))
import Linear.Affine (Point(P))

import Object.Door

data Object = FloorObject
            | WallObject
            | DoorObject DoorState

objectSprite :: Object -> (Int, Int)
objectSprite (FloorObject) = (1, 1)
objectSprite (WallObject)  = (9, 8)
objectSprite (DoorObject st) = case st of
    DoorStateOpened -> (8, 1)
    DoorStateClosed -> (7, 1)
