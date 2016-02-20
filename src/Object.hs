module Object where

import Message
import Sprite

data Object = MkObject
    { objSolid :: Bool
    , objSprite :: Sprite
    , objMsg :: Message -> ([Message], Object)
    }

defaultObject :: Object
defaultObject = MkObject
    { objSolid = True
    , objSprite = defaultSprite
    , objMsg = const ([], defaultObject)
    }
