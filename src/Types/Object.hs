{-# LANGUAGE OverloadedStrings #-}

module Types.Object
    ( Object(..)
    , defaultObject
    , fantasticObjMsg
    ) where

import Control.Monad.State
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON

import Types.Coordinate
import Types.Message
import Types.Sprite

data Object = MkObject
    { objFacing         :: Direction
    , objMass           :: Int
    , objMsg            :: Message -> State Object [Message]
    , objShipCoordinate :: Coordinate
    , objSolid          :: Bool
    , objSprite         :: Sprite
    , objInnerToJson    :: [JSON.Value]
    , objInnerFromJson  :: JSON.Value -> JSON.Result Object
    }

{-
instance JSON.FromJSON Object where
    parseJSON o = do
        mass   <- o JSON..: "mass"
        facing <- o JSON..: "facing"
        return $ defaultObject { objMass = mass{-, objFacing = facing -}}
-}

instance JSON.ToJSON Object where
    toJSON o = JSON.object $ [ "facing" JSON..= objFacing o
                             , "mass"   JSON..= objMass o
                             , "inner"  JSON..= objInnerToJson o
                             ]

defaultObject :: Object
defaultObject = MkObject
    { objFacing         = South
    , objMass           = 1
    , objMsg            = const $ return []
    , objShipCoordinate = coordinate 0 0
    , objSolid          = True
    , objSprite         = defaultSprite
    , objInnerToJson    = [JSON.Null]
    , objInnerFromJson  = const $ JSON.Success defaultObject
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
