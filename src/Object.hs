module Object where

import System.Message
import Sprite

type ObjectId = Integer

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
