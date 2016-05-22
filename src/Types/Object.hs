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
    { objFacing :: Direction
    , objSolid  :: Bool
    , objSprite :: Sprite
    , objMass   :: Integer
    , objMsg    :: Message -> ([Message], Object)
    } 

defaultObject :: Object
defaultObject = MkObject
    { objFacing = South
    , objSolid  = True
    , objSprite = defaultSprite
    , objMass   = 0
    , objMsg    = const ([], defaultObject)
    }

instance Show Object where
    show = const "{Object}"
