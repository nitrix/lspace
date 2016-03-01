module Demo
    ( demoObjects
    , demoContent
    ) where

import qualified Assoc as A
import Coordinate
import qualified Data.Map as M
import Object
import Object.Box
import Object.Player

demoObjects :: M.Map ObjectId Object
demoObjects = M.fromList
    [ (0, boxObject defaultObject defaultBox)
    , (1, boxObject defaultObject $ defaultBox { _boxLocked = True })
    , (2, playerObject defaultObject defaultPlayer)
    ]

demoContent :: A.Assoc Coordinate ObjectId
demoContent = A.fromList
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
    ]
