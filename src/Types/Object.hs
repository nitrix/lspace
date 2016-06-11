module Types.Object
    ( Object(..)
    , defaultObject
    , fantasticObjMsg
    ) where

import Control.Monad.State

import Types.Coordinate
import Types.Message
import Types.Sprite
import Types.Id

data Object = MkObject
    { objFacing     :: Direction
    , objId         :: ObjectId
    , objMass       :: Int
    , objMsg        :: Message -> State Object [Message]
    , objShipCoordinate :: Coordinate
    , objShipId     :: ShipId
    , objSolid      :: Bool
    , objSprite     :: Sprite
    } 

defaultObject :: Object
defaultObject = MkObject
    { objFacing     = South
    , objId         = 0
    , objMass       = 1
    , objMsg        = const $ return []
    , objShipCoordinate = coordinate 0 0
    , objShipId     = 0
    , objSolid      = True
    , objSprite     = defaultSprite
    }

-- TODO: Lenses for all of those!

-- This needs a better name
fantasticObjMsg :: (Message -> State inner [Message]) -- Object's message handler
                -> (Object -> inner -> Object)        -- Object's builder, trapping an `inner` type
                -> inner                              -- The `inner` in question
                -> Message -> State Object [Message]  -- Resulting type for the record field

fantasticObjMsg handler builder inner msg = do
        let (msgs, newInner) = runState (handler msg) inner
        o <- get
        put $ builder o newInner
        return msgs

instance Show Object where
    show o = "{Object #" ++ show (objId o) ++ "}"
