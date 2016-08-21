{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Object where

import qualified Data.Aeson as J
import GHC.Generics

import Control.Lens
import Coordinate
import Link
import Region
import Sprite

data ObjectCommon = MkObjectCommon
    { _objFacing :: Direction
    , _objMass   :: Int
    , _objRegion :: Link (Region Metric Object)
    } deriving (Generic, Eq, Ord)

data Object = MkObject ObjectCommon ObjectInfo deriving (Generic, Eq, Ord)
data ObjectInfo = ObjectBox    Box
                | ObjectFloor
                | ObjectPlant
                | ObjectPlayer Player
                | ObjectWall   Wall
                | ObjectUnknown
                deriving (Generic, Eq, Ord)

objRegion :: Lens' Object (Link (Region Metric Object))
objRegion = lens (\(MkObject common _) -> _objRegion common) (\(MkObject common info) x -> MkObject common { _objRegion = x } info)

objFacing :: Lens' Object Direction
objFacing = lens (\(MkObject common _) -> _objFacing common) (\(MkObject common info) x -> MkObject common { _objFacing = x } info)

data Box    = MkBox    { _boxState     :: BoxState } deriving (Generic, Eq, Ord)
data Player = MkPlayer { _playerHealth :: Int      } deriving (Generic, Eq, Ord, Show)
data Wall   = MkWall   { _wallType     :: WallType } deriving (Generic, Eq, Ord)

data BoxState = BoxClosed deriving (Generic, Eq, Ord)
data WallType = WallTypeHorizontal deriving (Generic, Eq, Ord)

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

instance Show Object where
    show _ = "{Object}"

objSprite :: Object -> Sprite
objSprite (MkObject _ (ObjectBox box)) = case _boxState box of
    BoxClosed -> sprite 0 2 ZOnGround
objSprite (MkObject _ ObjectFloor) = sprite 4 1 ZGround
objSprite (MkObject _ ObjectPlant) = sprite 0 1 ZOnGround
objSprite (MkObject common (ObjectPlayer _)) = case _objFacing common of
    North -> sprite 1 0 ZOnTop
    South -> sprite 1 2 ZOnTop
    West  -> sprite 1 1 ZOnTop
    East  -> sprite 1 3 ZOnTop
objSprite (MkObject _ (ObjectWall w)) = case _wallType w of
    WallTypeHorizontal -> sprite 5 2 ZGround
objSprite _ = defaultSprite

objSolid :: Object -> Bool
objSolid (MkObject _ (ObjectBox (MkBox BoxClosed))) = True
objSolid _ = False
