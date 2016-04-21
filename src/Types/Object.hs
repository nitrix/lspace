module Types.Object
    ( Object(..)
    , ObjectId
    , defaultObject
    ) where

import Types.Coordinate
import Types.Message
import Types.Sprite

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
    , objFacing = South
    , objSprite = defaultSprite
    , objMsg    = const ([], defaultObject)
    }

instance Show Object where
    show = const "{Object}"
