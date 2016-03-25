module Object
    ( Object(..)
    , ObjectId
    , defaultObject
    ) where

import Coordinate
import Message
import Sprite

type ObjectId = Integer

data Object = MkObject
    { objSolid  :: Bool
    , objFacing :: Direction
    , objSprite :: Sprite
    , objMsg    :: Message -> ([Message], Object)
    } 

defaultObject :: Object
defaultObject = MkObject
    { objSolid  = True
    , objFacing = DownDirection
    , objSprite = defaultSprite
    , objMsg    = const ([], defaultObject)
    }
