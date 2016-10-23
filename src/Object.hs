{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Object where

import qualified Data.Aeson as J
import GHC.Generics

import Control.Lens
import Coordinate
import Link
import Region
import Sprite

data ObjectCommon = MkObjectCommon
    { _objFacing     :: Direction
    , _objMass       :: Int
    , _objRegion     :: Link (Region Object)
    , _objCoordinate :: RegionCoordinate
    , _objFloodFill  :: Int
    } deriving (Generic, Eq, Ord)

data Object = MkObject ObjectCommon ObjectInfo deriving (Generic, Eq, Ord)
data ObjectInfo = ObjectBox    Box
                | ObjectFloor
                | ObjectPlant
                | ObjectPlayer Player
                | ObjectWall   Wall
                | ObjectUnknown
                deriving (Generic, Eq, Ord)

objCoordinate :: Lens' Object RegionCoordinate
objRegion     :: Lens' Object (Link (Region Object))
objFacing     :: Lens' Object Direction
objFloodFill  :: Lens' Object Int
objCoordinate = lens (\(MkObject common _) -> _objCoordinate common) (\(MkObject common info) x -> MkObject common { _objCoordinate = x } info)
objRegion     = lens (\(MkObject common _) -> _objRegion common)     (\(MkObject common info) x -> MkObject common { _objRegion     = x } info)
objFacing     = lens (\(MkObject common _) -> _objFacing common)     (\(MkObject common info) x -> MkObject common { _objFacing     = x } info)
objFloodFill  = lens (\(MkObject common _) -> _objFloodFill common)  (\(MkObject common info) x -> MkObject common { _objFloodFill  = x } info)

data Box    = MkBox    { _boxState     :: BoxState } deriving (Generic, Eq, Ord)
data Player = MkPlayer { _playerHealth :: Int      } deriving (Generic, Eq, Ord, Show)
data Wall   = MkWall   { _wallType     :: WallType } deriving (Generic, Eq, Ord)

data BoxState = BoxClosed deriving (Generic, Eq, Ord)
data WallType = WallTypeHorizontal deriving (Generic, Eq, Ord)

instance J.FromJSON Box
instance J.FromJSON BoxState
instance J.FromJSON Object
instance J.FromJSON ObjectInfo
instance J.FromJSON ObjectCommon where
    parseJSON (J.Object o) = do
        rFacing     <- o J..: "facing"
        rMass       <- o J..: "mass"
        rRegion     <- o J..: "region"
        rCoordinate <- o J..: "coordinate"
        return $ defaultObjectCommon
            { _objCoordinate = rCoordinate
            , _objFacing     = rFacing
            , _objMass       = rMass
            , _objRegion     = rRegion
            }
    parseJSON _ = error "Unable to parse ObjectCommon json"
instance J.FromJSON Player
instance J.FromJSON Wall
instance J.FromJSON WallType

instance J.ToJSON Box
instance J.ToJSON BoxState
instance J.ToJSON Object
instance J.ToJSON ObjectInfo
instance J.ToJSON ObjectCommon where
    toJSON obj = J.object
        [ "coordinate" J..= _objCoordinate obj
        , "facing"     J..= _objFacing     obj
        , "mass"       J..= _objMass       obj
        , "region"     J..= _objRegion     obj
        ]
instance J.ToJSON Player
instance J.ToJSON Wall
instance J.ToJSON WallType

instance Show Object where
    show _ = "{Object}"

defaultObjectCommon :: ObjectCommon
defaultObjectCommon = MkObjectCommon
    { _objFacing     = South
    , _objMass       = 1
    , _objRegion     = invalidLink
    , _objFloodFill  = 0
    , _objCoordinate = coordinate 0 0
    }
    
defaultBox :: Object
defaultBox = MkObject defaultObjectCommon (ObjectBox $ MkBox { _boxState = BoxClosed })

defaultWall :: Object
defaultWall = MkObject defaultObjectCommon (ObjectWall $ MkWall { _wallType = WallTypeHorizontal })

defaultFloor :: Object
defaultFloor = MkObject defaultObjectCommon ObjectFloor

objSprite :: Object -> Sprite
objSprite (MkObject _ (ObjectBox box)) = case _boxState box of
    BoxClosed -> sprite 4 2 ZOnGround
objSprite (MkObject _ ObjectFloor) = sprite 4 1 ZGround
objSprite (MkObject _ ObjectPlant) = sprite 0 1 ZOnGround
objSprite (MkObject common (ObjectPlayer _)) = case _objFacing common of
    North -> sprite 1 0 ZOnTop
    South -> sprite 1 2 ZOnTop
    West  -> sprite 1 1 ZOnTop
    East  -> sprite 1 3 ZOnTop
objSprite (MkObject _ (ObjectWall w)) = case _wallType w of
    WallTypeHorizontal -> sprite 5 1 ZGround
objSprite _ = defaultSprite

-- Player collision with it 
objSolid :: Object -> Bool
objSolid (MkObject _ (ObjectBox _)) = True
objSolid (MkObject _ (ObjectWall _)) = True
objSolid _ = False

-- Is holding a region together into a single component
objStructural :: Object -> Bool
objStructural (MkObject _ (ObjectWall _)) = True
objStructural (MkObject _ ObjectFloor) = True
objStructural _ = False
