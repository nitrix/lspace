{-# LANGUAGE TupleSections #-}

module Demo
    ( demoGame
    , demoShips
    ) where

import Control.Lens
import Linear (V2(V2))

import qualified Grid as G
import Types.Coordinate
import Types.Game
import Types.Link
import Types.Ship
import Types.World

demoShips :: WorldShips
demoShips = [demoAtlantis]

demoAtlantis :: Ship
demoAtlantis = MkShip
    { _shipCoordinate = coordinate 2 2
    , _shipGrid = G.fromList $
        [ (0, 0, LinkId 0)
        , (1, 0, LinkId 1)
        ]
    , _shipId        = 0
    , _shipMass      = 2
    , _shipVelocityX = 0
    , _shipVelocityY = 0
    , _shipDimension = V2 0 0
    }

demoGame :: Game
demoGame = defaultGame &~ do
    gamePlayer .= LinkId 1
    zoom gameWorld $ do
        worldShips .= demoShips

-- playerObject defaultObject defaultPlayer & \s -> s { objShipCoordinate = coordinate 1 0 
