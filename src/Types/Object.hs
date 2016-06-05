module Types.Object
    ( Object(..)
    , ObjectId
    , defaultObject
    ) where

import Types.Coordinate
import Types.Message
import Types.Sprite
import Types.Id

data Object = MkObject
    { objFacing :: Direction
    , objSolid  :: Bool
    , objSprite :: Sprite
    , objId     :: ObjectId
    , objMass   :: Integer
    , objMsg    :: Message -> ([Message], Object)
    } 

defaultObject :: Object
defaultObject = MkObject
    { objFacing = South
    , objSolid  = True
    , objId     = 0
    , objSprite = defaultSprite
    , objMass   = 0
    , objMsg    = const ([], defaultObject)
    }

instance Show Object where
    show o = "{Object #" ++ show (objId o) ++ "}"
