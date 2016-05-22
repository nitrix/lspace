module Ship where

import Grid
import Types.Coordinate
import Types.Object
import Linear (V2)

data Ship = MkShip
    { _shipGrid      :: Grid Coordinate ObjectId
    , _shipVelocityX :: Int
    , _shipVelocityY :: Int
    , _shipMass      :: Integer
    , _shipDimension :: V2 Integer
    }
