module World
    ( World
    , defaultWorld
    , worldObjectsAt
    , worldMoveObject
) where

import Coordinate
import Control.Lens
import Data.Maybe
import Demo
import Object
import System.Message
import qualified Assoc as A
import qualified Data.Map as M
import qualified Data.Set as S

type Layer = A.Assoc Coordinate ObjectId
type ObjectMapping = M.Map ObjectId Object

data World = MkWorld
    { _layer   :: Layer -- TODO: multiple layers
    , _objects :: ObjectMapping
    }

layer   :: Lens' World Layer
objects :: Lens' World ObjectMapping
layer   = lens _layer   (\s x -> s { _layer = x })
objects = lens _objects (\s x -> s { _objects = x })

defaultWorld :: World
defaultWorld = MkWorld
    { _layer   = demoContent
    , _objects = demoObjects
    }

worldObjectById :: World -> ObjectId -> Maybe Object
worldObjectById w oid = M.lookup oid $ view objects w

worldObjectIdsAt :: World -> Coordinate -> [ObjectId]
worldObjectIdsAt w c = S.toList $ A.lookup c $ view layer w

worldObjectsAt :: World -> Coordinate -> [Object]
worldObjectsAt w c = mapMaybe (worldObjectById w) (worldObjectIdsAt w c)

-- TODO: Needs to handle messag responses eventually
worldMessage :: Message -> ObjectId -> World -> World
worldMessage msg objid = objects %~ M.adjust newObject objid
    where
        newObject o = snd $ msgedObject o
        msgedObject o = objMsg o msg

-- TODO: Needs a serious rewrite eventually
worldMoveObject :: Direction -> ObjectId -> World -> World
worldMoveObject direction objid w =
    if (all (==False) $ map objSolid $ worldObjectsAt w newCoordinate)
    then msgOrientation . updateCoordinate $ w
    else msgOrientation w
    where
        msgOrientation z = worldMessage (MkMessage $ MovedMsg direction) objid z
        updateCoordinate = layer %~ A.adjustR (coordinateMove direction) objid
        currentCoordinate = S.elemAt 0 $ A.lookupR objid (w ^. layer)
        newCoordinate = coordinateMove direction currentCoordinate
