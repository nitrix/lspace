module Ship where

import Control.Lens
import qualified Grid as G
import Types.Object
import Linear (V2(V2))

data Ship = MkShip
    { _shipGrid      :: G.Grid Int ObjectId
    , _shipVelocityX :: Int
    , _shipVelocityY :: Int
    , _shipMass      :: Integer
    , _shipDimension :: V2 Integer
    }

defaultShip :: Ship
defaultShip = MkShip
    { _shipGrid      = G.empty
    , _shipMass      = 0
    , _shipVelocityX = 0
    , _shipVelocityY = 0
    , _shipDimension = V2 0 0
    }

shipGrid :: Lens' Ship (G.Grid Int ObjectId)
shipGrid = lens _shipGrid (\s x -> s { _shipGrid = x })
