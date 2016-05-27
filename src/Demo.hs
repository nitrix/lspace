{-# LANGUAGE TupleSections #-}

module Demo
    ( demoObjects
    , demoShips
    ) where

import qualified Data.Map as M
import Linear (V2(V2))
-- import Object.Box
-- import Object.Player
import Ship
import Types.Coordinate
import Types.Object
import Types.World

import qualified Grid as G

demoShips :: WorldShips
demoShips = [(coordinate 2 2, demoAtlantis)]

demoAtlantis :: Ship
demoAtlantis = MkShip
    { _shipGrid = G.fromList $
        [ (x, y, 0) | x <- [1..1000], y <- [0..1000]]
    , _shipMass = 0
    , _shipVelocityX = 0
    , _shipVelocityY = 0
    , _shipDimension = V2 0 0
    }

demoObjects :: WorldObjects
demoObjects = M.fromList $
    [ (0, defaultObject)
    , (1, defaultObject)
    , (2, defaultObject)
    ]
