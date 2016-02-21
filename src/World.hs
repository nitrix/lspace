module World
    ( World
    , defaultWorld
    , worldEntities
    , worldObjectsAt
    , worldTestInteractAll
    , playerMoveUp
) where

import Coordinate
import Control.Monad
import qualified Data.Map as M
import Data.Maybe

import Message
import Object
import Object.Box
import Object.Player

data World = MkWorld
    { layers   :: [M.Map Coordinate [ObjectId]]
    , objects  :: M.Map ObjectId Object  
    , entities :: M.Map ObjectId Coordinate
    }

getObjectById :: World -> ObjectId -> Maybe Object
getObjectById w oid = M.lookup oid (objects w)

defaultWorld :: World
defaultWorld = MkWorld
    { layers = [demoLayer]
    , objects = demoObjects
    , entities = demoEntities
    }

demoObjects :: M.Map ObjectId Object
demoObjects = M.fromList
    [ (0, boxObject defaultObject defaultBox)
    , (1, boxObject defaultObject $ defaultBox { boxState = BoxClosedLocked })
    , (2, playerObject defaultObject defaultPlayer)
    ]

demoLayer :: M.Map Coordinate [ObjectId]
demoLayer = M.fromList
    [ (coordinate 0 0, [0])
    , (coordinate 1 0, [0])
    , (coordinate 0 1, [0])
    , (coordinate 2 1, [1])
    , (coordinate 1 2, [1])
    , (coordinate 3 1, [0])
    , (coordinate 0 0, [1])
    , (coordinate 1 3, [0])
    , (coordinate 5 2, [0])
    ]

demoEntities :: M.Map ObjectId Coordinate
demoEntities = M.fromList
    [ (2, coordinate 5 5)
    ]

worldEntities :: World -> [(Coordinate, Object)]
worldEntities w = map (\(x,y) -> (x, fromJust y)) $ filter (\(_,y) -> isJust y) $ (\x -> (snd x, getObjectById w $ fst x)) <$> (M.toList $ entities w)

worldObjectsAt :: World -> Coordinate -> [Object]
worldObjectsAt w c = catMaybes $ map (getObjectById w) $ join $ mapMaybe (M.lookup c) (layers w)

-- TODO
worldTestInteractAll :: World -> World
worldTestInteractAll w = w { objects = go $ objects w }
    where
        go :: M.Map ObjectId Object -> M.Map ObjectId Object
        go objs = (\o -> snd $ objMsg o InteractMsg) <$> objs -- output msgs are discarded

-- TODO
playerMoveUp :: World -> World
playerMoveUp = id
