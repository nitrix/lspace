module Object.Plant
    ( plantObject
    , defaultPlant
    , Plant(..)
    )
where

import Control.Monad.State
import Message
import Object
import Sprite

data Plant = MkPlant

plantObject :: Object -> Plant -> Object
plantObject obj f = obj
    { objSolid  = False
    , objSprite = plantSprite f
    , objMsg    = \msg -> plantObject obj <$> runState (plantMsg msg) f

    }

defaultPlant :: Plant
defaultPlant = MkPlant 

plantSprite :: Plant -> Sprite
plantSprite _ = sprite 0 1 ZOnGround

plantMsg :: Message -> State Plant [Message]
plantMsg _ = return []
