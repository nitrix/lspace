{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.Ship where

import Control.Lens
import Linear (V2(V2))
import GHC.Generics
import qualified Data.Aeson as J
import qualified Data.Map as M

import qualified Grid as G
import Types.Coordinate
import Types.Link
import Types.Object

type ShipGrid = G.Grid Int (Link Object)
data Ship = MkShip
    { _shipCoordinate :: Coordinate
    , _shipGrid       :: ShipGrid
    , _shipId         :: Int
    , _shipVelocityX  :: Int
    , _shipVelocityY  :: Int
    , _shipMass       :: Int
    , _shipDimension  :: V2 Integer
    } deriving Generic

defaultShip :: Ship
defaultShip = MkShip
    { _shipCoordinate = coordinate 0 0
    , _shipGrid       = G.empty
    , _shipId         = 0
    , _shipMass       = 0
    , _shipVelocityX  = 0
    , _shipVelocityY  = 0
    , _shipDimension  = V2 0 0
    }

instance J.FromJSON Ship
instance J.FromJSON ShipGrid
instance J.ToJSON Ship
instance J.ToJSON ShipGrid

shipGrid :: Lens' Ship (G.Grid Int (Link Object))
shipGrid = lens _shipGrid (\s x -> s { _shipGrid = x })

shipCoordinate :: Lens' Ship Coordinate
shipId         :: Lens' Ship Int
shipMass       :: Lens' Ship Int
shipCoordinate = lens _shipCoordinate (\s x -> s { _shipCoordinate = x })
shipId         = lens _shipId         (\s x -> s { _shipId         = x })
shipMass       = lens _shipMass       (\s x -> s { _shipMass       = x })
