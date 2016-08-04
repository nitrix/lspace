{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Ship where

import Control.Lens (Lens', lens)
import GHC.Generics
import Data.Aeson

import qualified Grid as G
import Coordinate
import Link

data Ship k v = MkShip
    { _shipCoordinate :: Coordinate
    , _shipDimension  :: (k, k)
    , _shipGrid       :: G.Grid k (Link v)
    , _shipMass       :: k
    , _shipVelocity   :: (k, k)
    } deriving (Show, Generic)

defaultShip :: Num k => Ship k v
defaultShip = MkShip
    { _shipCoordinate = coordinate 0 0
    , _shipDimension  = (0, 0)
    , _shipGrid       = G.empty
    , _shipMass       = 0
    , _shipVelocity   = (0, 0)
    }

instance (Linked v, FromJSON k, Show k, Integral k, Ord (Link v)) => FromJSON (Ship k v) where
    parseJSON (Object o) = do
        sCoord     <- o .: "coordinate"
        sDimension <- o .: "dimension"
        sGrid      <- o .: "grid"
        sMass      <- o .: "mass"
        sVelocity  <- o .: "velocity"
        return $ MkShip
            { _shipCoordinate = sCoord
            , _shipGrid       = sGrid
            , _shipVelocity   = sVelocity
            , _shipMass       = sMass
            , _shipDimension  = sDimension
            }
    parseJSON _ = error "Unable to parse Ship json"

instance (ToJSON k, Integral k) => ToJSON (Ship k v) where
    toJSON s = object
        [ "coordinate" .= _shipCoordinate s
        , "grid"       .= _shipGrid s
        , "mass"       .= _shipMass s
        , "velocity"   .= _shipVelocity s
        , "dimension"  .= _shipDimension s
        ]

shipGrid       :: Lens' (Ship k v) (G.Grid k (Link v))
shipGrid       = lens _shipGrid (\s x -> s { _shipGrid = x })
shipCoordinate :: Lens' (Ship k v) Coordinate
shipCoordinate = lens _shipCoordinate (\s x -> s { _shipCoordinate = x })
shipMass       :: Lens' (Ship k v) k
shipMass       = lens _shipMass       (\s x -> s { _shipMass       = x })