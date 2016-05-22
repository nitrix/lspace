{-# LANGUAGE TupleSections #-}

module Demo
    ( demoObjects
    , demoShips
    ) where

import qualified Data.Map as M
import qualified Grid as G
import Linear (V2(V2))
-- import Object.Box
-- import Object.Player
import Ship
import Types.Coordinate
import Types.Object
import Types.World

demoShips :: WorldShips
demoShips = [(coordinate 0 0, demoAtlantis)]

demoAtlantis :: Ship
demoAtlantis = MkShip
    { _shipGrid = G.fromList $
        [ (coordinate 0 0, 0)
        , (coordinate 1 1, 1)
        , (coordinate 2 2, 2)
        ] -- ++ [(coordinate x y,0) | x <- [10..200], y <- [10..200]]
    , _shipMass = 3
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
