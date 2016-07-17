{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Ship where

import Control.Lens (Lens', lens, view)
import Linear (V2(V2), _x, _y)
import GHC.Generics
import Data.Aeson

import qualified Grid as G
import Types.Coordinate
import Types.Link
import qualified Types.Object as O

data Ship = MkShip
    { _shipCoordinate :: Coordinate
    , _shipGrid       :: G.Grid Int (Link O.Object)
    , _shipVelocity   :: V2 Int
    , _shipMass       :: Int
    , _shipDimension  :: V2 Integer
    } deriving Generic

defaultShip :: Ship
defaultShip = MkShip
    { _shipCoordinate = coordinate 0 0
    , _shipGrid       = G.empty
    , _shipMass       = 0
    , _shipVelocity   = V2 0 0
    , _shipDimension  = V2 0 0
    }

instance FromJSON Ship where
    parseJSON (Object o) = do
        sCoord     <- o .: "coordinate"
        sMass      <- o .: "mass"
        -- sVelocity  <- o .: "velocity"
        -- sDimension <- o .: "dimension"
        return $ MkShip
            { _shipCoordinate = sCoord
            , _shipGrid       = G.empty -- TODO
            -- , _shipVelocity   = sVelocity
            , _shipMass       = sMass
            -- , _shipDimension  = sDimension
            }
    parseJSON _ = error "Unable to parse Ship json"

instance ToJSON Ship where
    toJSON s = object
        [ "coordinate" .= _shipCoordinate s
        -- , "grid"       .= _shipGrid s -- TODO
        , "mass"       .= _shipMass s
        , "velocity"   .= object
            [ "x" .= (view _x $ _shipVelocity s)
            , "y" .= (view _y $ _shipVelocity s)
            ]
        , "dimension"  .= object
            [ "x" .= (view _x $ _shipDimension s)
            , "y" .= (view _y $ _shipDimension s)
            ]
        ]

shipGrid :: Lens' Ship (G.Grid Int (Link O.Object))
shipGrid = lens _shipGrid (\s x -> s { _shipGrid = x })

shipCoordinate :: Lens' Ship Coordinate
shipMass       :: Lens' Ship Int
shipCoordinate = lens _shipCoordinate (\s x -> s { _shipCoordinate = x })
shipMass       = lens _shipMass       (\s x -> s { _shipMass       = x })
