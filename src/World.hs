module World
    ( World
    , demoContent
    , defaultWorld
    , worldObjectsAt
    , worldTestInteractAll
    , thingMove
) where

import qualified Assoc as A
import Coordinate
import Control.Lens
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Message
import Object
import Object.Box
import Object.Player

type ObjectMapping = M.Map ObjectId Object
type ContentAssoc = A.Assoc Coordinate ObjectId

data World = MkWorld
    { _content :: ContentAssoc
    , _objects :: ObjectMapping
    }

content :: Lens' World ContentAssoc
content f s = (\x -> s { _content = x }) <$> f (_content s)

objects :: Lens' World ObjectMapping
objects f s = (\x -> s { _objects = x }) <$> f (_objects s)

getObjectById :: World -> ObjectId -> Maybe Object
getObjectById w oid = M.lookup oid (_objects w)

defaultWorld :: World
defaultWorld = MkWorld
    { _content = demoContent
    , _objects = demoObjects
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
worldObjectsAt w c = catMaybes $ map (getObjectById w) $ S.toList $ A.lookup c (_content w)

-- worldMapObjects :: World -> (Object -> Object) 
-- TODO
worldTestInteractAll :: World -> World
worldTestInteractAll = objects %~ go
    where
        go :: M.Map ObjectId Object -> M.Map ObjectId Object
        go objs = (\o -> snd $ objMsg o InteractMsg) <$> objs -- output msgs are discarded

-- Needs to handle messag responses eventually
worldMessage :: Message -> ObjectId -> World -> World
worldMessage msg objid = objects %~ M.adjust newObject objid
    where
        newObject o = snd $ msgedObject o
        msgedObject o = objMsg o msg

thingMove :: Direction -> ObjectId -> World -> World
thingMove direction objid = msgOrientation . updateCoordinate
    where
        msgOrientation w = worldMessage (MovedMsg direction) objid w
        updateCoordinate = content %~ A.adjustR (coordinateMove direction) objid
