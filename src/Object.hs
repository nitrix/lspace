module Object where

import Message

data Object = MkObject
    { objSolid :: Bool
    , objSprite :: (Int, Int)
    , objMsg :: Message -> [Message]
    , objUpdate :: Message -> Object
    }

defaultObject :: Object
defaultObject = MkObject
    { objSolid = True
    , objSprite = (0, 0)
    , objMsg = const []
    , objUpdate = const defaultObject
    }
