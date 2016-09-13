{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Region where

import Control.Lens (Lens', lens)
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

defaultRegion :: Region v
defaultRegion = MkRegion
    { _regionCoordinate = coordinate 0 0
    , _regionDimension  = (0, 0)
    , _regionGrid       = G.empty
    , _regionMass       = 0
    , _regionVelocity   = (0, 0)
    }

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

regionGrid       :: Lens' (Region v) (G.Grid Int (Link v))
regionCoordinate :: Lens' (Region v) WorldCoordinate
regionMass       :: Lens' (Region v) Int
regionGrid       = lens _regionGrid       (\s x -> s { _regionGrid       = x })
regionCoordinate = lens _regionCoordinate (\s x -> s { _regionCoordinate = x })
regionMass       = lens _regionMass       (\s x -> s { _regionMass       = x })
