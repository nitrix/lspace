{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Object where

import qualified Data.Aeson as J
import GHC.Generics

import Types.Coordinate
import Types.Sprite

data ObjectCommon = MkObjectCommon
    { objFacing         :: Direction
    , objMass           :: Int
    , objShipCoordinate :: Coordinate
    , objSolid          :: Bool
    } deriving Generic

data Object = MkObject ObjectCommon ObjectInfo deriving Generic
data ObjectInfo = ObjectBox     Box
                | ObjectFloor
                | ObjectPlant
                | ObjectPlayer  Player
                | ObjectWall    Wall
                | ObjectUnknown
                deriving Generic

data Box      = MkBox    { _boxState     :: BoxState } deriving Generic
data Player   = MkPlayer { _playerHealth :: Int }      deriving Generic
data Wall     = MkWall   { _wallType     :: WallType } deriving Generic

data BoxState = BoxOpened | BoxClosed deriving Generic
data WallType = WallTypeHorizontal deriving Generic

instance J.FromJSON Box
instance J.FromJSON BoxState
instance J.FromJSON Object
instance J.FromJSON ObjectInfo
instance J.FromJSON ObjectCommon
instance J.FromJSON Player
instance J.FromJSON Wall
instance J.FromJSON WallType

instance J.ToJSON Box
instance J.ToJSON BoxState
instance J.ToJSON Object
instance J.ToJSON ObjectInfo
instance J.ToJSON ObjectCommon
instance J.ToJSON Player
instance J.ToJSON Wall
instance J.ToJSON WallType

defaultObject :: Object
defaultObject = MkObject
    ( MkObjectCommon
    { objFacing         = South
    , objMass           = 1
    , objShipCoordinate = coordinate 0 0
    , objSolid          = True
    } )
    ObjectUnknown

instance Show Object where
    show _ = "{Object}"
