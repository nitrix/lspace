module Object.Plant
    ( plantObject
    , defaultPlant
    , Plant(..)
    )
where

import Control.Monad.State

import Types.Message
import Types.Object
import Types.Sprite

data Plant = MkPlant

plantObject :: Object -> Plant -> Object
plantObject obj f = obj
    { objSolid  = False
    , objSprite = plantSprite f
    , objMsg    = fantasticObjMsg plantMsg plantObject f
    }

defaultPlant :: Plant
defaultPlant = MkPlant 

plantSprite :: Plant -> Sprite
plantSprite _ = sprite 0 1 ZOnGround

plantMsg :: Message -> State Plant [Message]
plantMsg _ = return []
