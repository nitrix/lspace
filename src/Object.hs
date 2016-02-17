module Object where

import Message

data Object = MkObject
    { objId :: Integer
    , objSprite :: (Int, Int)
    , objUpdate :: Message -> Object
    }

defaultObject :: Object
defaultObject = MkObject
    { objId = 0
    , objSprite = (0, 0)
    , objUpdate = const defaultObject
    }
