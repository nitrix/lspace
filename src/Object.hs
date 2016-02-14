module Object where

import Linear (V2(V2))
import Linear.Affine (Point(P))
import Foreign.C.Types

import Object.Door

data Object = FloorObject
            | WallObject
            | DoorObject DoorState

objectSprite :: Object -> Point V2 CInt
objectSprite (FloorObject) = P $ V2 1 1 * V2 32 32
objectSprite (WallObject)  = P $ V2 9 8 * V2 32 32
objectSprite (DoorObject st) = case st of
    DoorStateOpened -> P $ V2 8 1 * V2 32 32
    DoorStateClosed -> P $ V2 7 1 * V2 32 32
