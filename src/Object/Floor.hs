module Object.Floor
    ( floorObject
    , defaultFloor
    , Floor(..)
    )
where

import Control.Monad.State
import Message
import Object
import Sprite

data Floor = MkFloor

floorObject :: Object -> Floor -> Object
floorObject obj f = obj
    { objSolid  = False
    , objSprite = floorSprite f
    , objMsg    = \msg -> floorObject obj <$> runState (floorMsg msg) f
    }

defaultFloor :: Floor
defaultFloor = MkFloor 

floorSprite :: Floor -> Sprite
floorSprite _ = sprite 4 1 ZGround

floorMsg :: Message -> State Floor [Message]
floorMsg _ = return []
