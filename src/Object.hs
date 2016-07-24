{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Object where

import qualified Data.Aeson as J
import GHC.Generics

import Coordinate
import Sprite

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

data BoxState = BoxClosed deriving Generic
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

objSprite :: Object -> Sprite
objSprite (MkObject _ (ObjectBox box)) = case _boxState box of
    BoxClosed -> sprite 0 0 ZOnGround
objSprite (MkObject _ ObjectFloor) = sprite 4 1 ZGround
objSprite (MkObject _ ObjectPlant) = sprite 0 1 ZOnGround
objSprite (MkObject common (ObjectPlayer _)) = case objFacing common of
    North -> sprite 1 0 ZOnTop
    South -> sprite 1 2 ZOnTop
    West  -> sprite 1 1 ZOnTop
    East  -> sprite 1 3 ZOnTop
objSprite (MkObject _ (ObjectWall w)) = case _wallType w of
    WallTypeHorizontal -> sprite 5 2 ZGround
objSprite _ = defaultSprite
