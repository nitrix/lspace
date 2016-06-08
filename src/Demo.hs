{-# LANGUAGE TupleSections #-}

module Demo
    ( demoGame
    , demoObjects
    , demoShips
    ) where

import Control.Lens
import qualified Data.Map as M
import Linear (V2(V2))

import qualified Grid as G
import Object.Player
import Types.Coordinate
import Types.Game
import Types.Object
import Types.Ship
import Types.World

demoShips :: WorldShips
demoShips = M.fromList $ [(0, demoAtlantis)]

demoAtlantis :: Ship
demoAtlantis = MkShip
    { _shipCoordinate = coordinate 0 0
    , _shipGrid = G.fromList $
        [ (0, 0, 0)
        , (1, 0, 1)
        ]
    , _shipId        = 1
    , _shipMass      = 2
    , _shipVelocityX = 0
    , _shipVelocityY = 0
    , _shipDimension = V2 0 0
    }

demoObjects :: WorldObjects
demoObjects = M.fromList $
    [ (0, defaultObject & \s -> s { objId = 0, objShipId = 1 })
    , (1, playerObject defaultObject defaultPlayer & \s -> s { objCoordinate = coordinate 0 1, objId = 1, objShipId = 1 })
    ]

demoGame :: Game
demoGame = defaultGame &~ do
    gamePlayer .= 1
    zoom gameWorld $ do
        worldShips      .= demoShips
        worldObjects    .= demoObjects
        worldNextObjId  .= 2
        worldNextShipId .= 2
