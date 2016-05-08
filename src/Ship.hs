module Ship where

import Grid
import Types.Coordinate
import Types.Object

data Ship = MkShip
    { _shipGrid     :: Grid Coordinate ObjectId
    , _shipVelocity :: Integer
    , _shipMass     :: Integer
    }
