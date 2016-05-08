{-# LANGUAGE TupleSections #-}

module Demo
    ( demoShips
    ) where

import qualified Data.Map as M
import qualified Grid as G
-- import Object.Box
-- import Object.Player
import Ship
import Types.Coordinate
import Types.World

demoShips :: WorldShips
demoShips = M.singleton (coordinate 0 0) demoAtlantis

demoAtlantis :: Ship
demoAtlantis = MkShip
    { _shipGrid = G.fromList $
        [ (coordinate 0 0, 0)
        , (coordinate 1 1, 1)
        , (coordinate 2 2, 2)
        ] -- ++ [(coordinate x y,0) | x <- [10..200], y <- [10..200]]
    , _shipVelocity = 0
    , _shipMass = 3
    }
