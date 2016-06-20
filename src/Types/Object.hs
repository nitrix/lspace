{-# LANGUAGE OverloadedStrings #-}

module Types.Object
    ( Object(..)
    , defaultObject
    , fantasticObjMsg
    ) where

import Control.Monad.State
import qualified Data.Aeson as J

import Types.Coordinate
import Types.Message
import Types.Sprite

data Object = MkObject
    { objFacing         :: Direction
    , objInnerToJson    :: J.Value
    , objInnerFromJson  :: J.Value -> J.Result Object
    , objMass           :: Int
    , objMsg            :: Message -> State Object [Message]
    , objShipCoordinate :: Coordinate
    , objSolid          :: Bool
    , objSprite         :: Sprite
    , objType           :: String
    }

instance J.FromJSON Object where
    parseJSON (J.Object o) = do
        theMass   <- o J..: "mass"
        theFacing <- o J..: "facing"
        return $ defaultObject { objMass = theMass, objFacing = theFacing }
    parseJSON _ = error "Unable to parse the JSON for Object"

instance J.ToJSON Object where
    toJSON o = J.object $ [ "facing" J..= objFacing o
                          , "mass"   J..= objMass o
                          , "inner"  J..= objInnerToJson o
                          ]

defaultObject :: Object
defaultObject = MkObject
    { objFacing         = South
    , objInnerToJson    = J.Null
    , objInnerFromJson  = const $ J.Success defaultObject
    , objMass           = 1
    , objMsg            = const $ return []
    , objShipCoordinate = coordinate 0 0
    , objSolid          = True
    , objSprite         = defaultSprite
    , objType           = "default"
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
    show _ = "{Object}"
