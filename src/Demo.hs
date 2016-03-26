module Demo
    ( demoObjects
    , demoLayer
    , demoPlayer
    ) where

import qualified Assoc as A
import Coordinate
import qualified Data.Map as M
import Object
import Object.Box
import Object.Player
import World

demoObjects :: WorldObjects
demoObjects = M.fromList
    [ (0, boxObject defaultObject defaultBox)
    , (1, boxObject defaultObject defaultBox)
    , (2, playerObject defaultObject defaultPlayer)
    ]

demoLayer :: WorldLayer
demoLayer = A.fromList $
    [ (coordinate 0 0, 0)
    , (coordinate 1 0, 0)
    , (coordinate 0 1, 0)
    , (coordinate 2 1, 1)
    , (coordinate 1 2, 0)
    , (coordinate 3 1, 0)
    , (coordinate 0 0, 1)
    , (coordinate 1 3, 1)
    , (coordinate 5 2, 0)
    , (coordinate 5 5, 0)
    , (coordinate 5 6, 2)
    ] -- ++ [(coordinate x y,0) | x <- [10..200], y <- [10..200]]

demoPlayer :: ObjectId
demoPlayer = 2
