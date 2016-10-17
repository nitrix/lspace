{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Region where

import Control.Lens (makeLenses)
import GHC.Generics
import Data.Aeson

import qualified Grid as G
import Coordinate
import Link

data Region v = MkRegion
    { _regionCoordinate :: WorldCoordinate
    , _regionDimension  :: (Int, Int)
    , _regionGrid       :: G.Grid Int (Link v)
    , _regionMass       :: Int
    , _regionVelocity   :: (Int, Int)
    } deriving (Show, Generic)

makeLenses ''Region

instance Linkable v => FromJSON (Region v) where
    parseJSON (Object o) = do
        rCoord     <- o .: "coordinate"
        rDimension <- o .: "dimension"
        rGrid      <- o .: "grid"
        rMass      <- o .: "mass"
        rVelocity  <- o .: "velocity"
        return $ MkRegion
            { _regionCoordinate = rCoord
            , _regionGrid       = rGrid
            , _regionVelocity   = rVelocity
            , _regionMass       = rMass
            , _regionDimension  = rDimension
            }
    parseJSON _ = error "Unable to parse Region json"

instance ToJSON (Region v) where
    toJSON s = object
        [ "coordinate" .= _regionCoordinate s
        , "grid"       .= _regionGrid s
        , "mass"       .= _regionMass s
        , "velocity"   .= _regionVelocity s
        , "dimension"  .= _regionDimension s
        ]

defaultRegion :: Region v
defaultRegion = MkRegion
    { _regionCoordinate = coordinate 0 0
    , _regionDimension  = (0, 0)
    , _regionGrid       = G.empty
    , _regionMass       = 0
    , _regionVelocity   = (0, 0)
    }
