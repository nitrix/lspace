module Object.Door (doorObject, defaultDoor) where

import Message
import Object

data DoorState = DoorOpened | DoorClosed | DoorClosedLocked

data Door = MkDoor
    { doorState :: DoorState
    , doorPin   :: Int
    }

doorObject :: Door -> Object
doorObject x = defaultObject
    { objSprite = doorSprite x
    , objUpdate = doorObject . doorUpdate x
    }

defaultDoor :: Door
defaultDoor = MkDoor 
    { doorState = DoorClosed
    , doorPin = 0
    }

doorSprite :: Door -> (Int, Int)
doorSprite x = case doorState x of
    DoorOpened       -> opened
    DoorClosed       -> closed
    DoorClosedLocked -> closed
    where
        opened = (8, 1)
        closed = (7, 1)

doorUpdate :: Door -> Message -> Door
doorUpdate x msg = case msg of
    InteractMsg -> case doorState x of
        DoorOpened       -> x { doorState = DoorClosed }
        DoorClosed       -> x { doorState = DoorOpened }
        DoorClosedLocked -> x
    _ -> x
