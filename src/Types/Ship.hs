{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Ship where

import Control.Lens (Lens', lens)
import GHC.Generics
import Data.Aeson

import qualified Grid as G
import Types.Coordinate
import Types.Link
import qualified Types.Object as O

data Ship = MkShip
    { _shipCoordinate :: Coordinate
    , _shipDimension  :: (Int, Int)
    , _shipGrid       :: G.Grid Int (Link O.Object)
    , _shipMass       :: Int
    , _shipVelocity   :: (Int, Int)
    } deriving (Show, Generic)

defaultShip :: Ship
defaultShip = MkShip
    { _shipCoordinate = coordinate 0 0
    , _shipDimension  = (0, 0)
    , _shipGrid       = G.empty
    , _shipMass       = 0
    , _shipVelocity   = (0, 0)
    }

instance FromJSON Ship where
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

instance ToJSON Ship where
    toJSON s = object
        [ "coordinate" .= _shipCoordinate s
        , "grid"       .= _shipGrid s
        , "mass"       .= _shipMass s
        , "velocity"   .= _shipVelocity s
        , "dimension"  .= _shipDimension s
        ]

shipGrid :: Lens' Ship (G.Grid Int (Link O.Object))
shipGrid = lens _shipGrid (\s x -> s { _shipGrid = x })

shipCoordinate :: Lens' Ship Coordinate
shipMass       :: Lens' Ship Int
shipCoordinate = lens _shipCoordinate (\s x -> s { _shipCoordinate = x })
shipMass       = lens _shipMass       (\s x -> s { _shipMass       = x })
