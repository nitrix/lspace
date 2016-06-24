{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Ship where

import Control.Lens (Lens', lens)
import Linear (V2(V2))
import GHC.Generics
import Data.Aeson

import qualified Grid as G
import Types.Coordinate
import Types.Link
import qualified Types.Object as O

data Ship = MkShip
    { _shipCoordinate :: Coordinate
    , _shipGrid       :: G.Grid Int (Link O.Object)
    , _shipId         :: Int
    , _shipVelocity   :: V2 Int
    , _shipMass       :: Int
    , _shipDimension  :: V2 Integer
    } deriving Generic

defaultShip :: Ship
defaultShip = MkShip
    { _shipCoordinate = coordinate 0 0
    , _shipGrid       = G.empty
    , _shipId         = 0
    , _shipMass       = 0
    , _shipVelocity   = V2 0 0
    , _shipDimension  = V2 0 0
    }

instance FromJSON Ship where
    parseJSON (Object o) = do
        sCoord     <- o .: "coordinate"
        sId        <- o .: "id"
        sMass      <- o .: "mass"
        sVelocity  <- o .: "velocity"
        sDimension <- o .: "dimension"
        return $ MkShip
            { _shipCoordinate = sCoord
            , _shipGrid       = G.empty -- TODO, id system
            , _shipId         = sId
            , _shipVelocity   = sVelocity
            , _shipMass       = sMass
            , _shipDimension  = sDimension
            }
    parseJSON _ = error "Unable to parse Ship json"

instance ToJSON Ship where
    toJSON s = object
        [ "coordinate" .= _shipCoordinate s
        , "id"         .= _shipId s
        , "mass"       .= _shipMass s
        , "velocity"   .= _shipVelocity s
        , "dimension"  .= _shipDimension s
        ]

shipGrid :: Lens' Ship (G.Grid Int (Link O.Object))
shipGrid = lens _shipGrid (\s x -> s { _shipGrid = x })

shipCoordinate :: Lens' Ship Coordinate
shipId         :: Lens' Ship Int
shipMass       :: Lens' Ship Int
shipCoordinate = lens _shipCoordinate (\s x -> s { _shipCoordinate = x })
shipId         = lens _shipId         (\s x -> s { _shipId         = x })
shipMass       = lens _shipMass       (\s x -> s { _shipMass       = x })
