module World
    ( World
    , demoContent
    , defaultWorld
    , worldObjectsAt
    , worldTestInteractAll
    , selfMoveUp
) where

import qualified Assoc as A
import Coordinate
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Message
import Object
import Object.Box
import Object.Player

data World = MkWorld
    { content :: A.Assoc Coordinate ObjectId
    , objects :: M.Map ObjectId Object  
    }

getObjectById :: World -> ObjectId -> Maybe Object
getObjectById w oid = M.lookup oid (objects w)

defaultWorld :: World
defaultWorld = MkWorld
    { content = demoContent
    , objects = demoObjects
    }

demoObjects :: M.Map ObjectId Object
demoObjects = M.fromList
    [ (0, boxObject defaultObject defaultBox)
    , (1, boxObject defaultObject $ defaultBox { boxLocked = True })
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
    , (coordinate 5 5, 2)
    ]

worldObjectsAt :: World -> Coordinate -> [Object]
worldObjectsAt w c = catMaybes $ map (getObjectById w) $ S.toList $ A.lookup c (content w)

-- TODO
worldTestInteractAll :: World -> World
worldTestInteractAll w = w { objects = go $ objects w }
    where
        go :: M.Map ObjectId Object -> M.Map ObjectId Object
        go objs = (\o -> snd $ objMsg o InteractMsg) <$> objs -- output msgs are discarded

selfMoveUp :: ObjectId -> World -> World
selfMoveUp obj w = w
-- selfMoveUp objid w = w { content = A.adjustR _ objid (content w) }
