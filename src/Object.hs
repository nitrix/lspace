module Object where

import Message

data Object = MkObject
    { objSolid :: Bool
    , objSprite :: (Int, Int)
    , objUpdate :: Message -> ([Message], Object)
    }

defaultObject :: Object
defaultObject = MkObject
    { objSolid = True
    , objSprite = (0, 0)
    , objUpdate = const ([], defaultObject)
    }
