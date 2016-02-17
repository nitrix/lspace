module Object.Door (doorObject, defaultDoor) where

import Message
import Object

data DoorState = DoorOpened | DoorClosed

data Door = MkDoor
    { doorState :: DoorState
    , doorPin   :: Int
    }

doorObject :: Door -> Object
doorObject d = defaultObject
    { objSprite = doorSprite d
    , objUpdate = doorObject . doorUpdate d
    }

defaultDoor :: Door
defaultDoor = MkDoor 
    { doorState = DoorClosed
    , doorPin = 0
    }

doorSprite :: Door -> (Int, Int)
doorSprite d = case doorState d of
    DoorOpened -> (8, 1)
    DoorClosed -> (7, 1)

doorUpdate :: Door -> Message -> Door
doorUpdate d msg = case msg of
    InteractMsg -> case doorState d of
        DoorOpened -> d { doorState = DoorClosed }
        DoorClosed -> d { doorState = DoorOpened }
    _ -> d
