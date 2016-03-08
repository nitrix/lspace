module World
    ( World
    , defaultWorld
    , worldObjectsAt
    , worldMoveObject
) where

import qualified Assoc as A
import Coordinate
import Control.Lens
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Demo
import Object
import System.Message

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

worldObjectsAt :: World -> Coordinate -> [Object]
worldObjectsAt w c = mapMaybe resolveObjectIds objectIds
    where
        resolveObjectIds :: ObjectId -> Maybe Object
        resolveObjectIds oid = M.lookup oid $ view objects w
        objectIds :: [ObjectId]
        objectIds = S.toList $ A.lookup c $ view layer w

-- TODO: Needs to handle messag responses eventually
worldMessage :: Message -> World -> World
worldMessage msg = objects %~ M.adjust newObject objid
    where
        newObject o = snd $ msgedObject o
        msgedObject o = objMsg o msg
        objid = msgTo msg

-- TODO: Possible replacement; but needs proper msgTo and msgFrom as it's passing data around
worldMessage' :: Message -> World -> World
worldMessage' m = objects %~ updateObjects m
    where
        updateObjects :: Message -> ObjectMapping -> ObjectMapping
        updateObjects m objs = case msgedObj of
                Just (msgs, obj) -> foldr updateObjects (M.insert objId obj objs) msgs
                Nothing          -> objs
            where
                objId = msgTo m
                msgedObj :: Maybe ([Message], Object)
                msgedObj = flip objMsg m <$> M.lookup objId objs

-- TODO: Needs a serious rewrite eventually
worldMoveObject :: Direction -> ObjectId -> World -> World
worldMoveObject direction objid w =
    if (all (==False) $ map objSolid $ worldObjectsAt w newCoordinate)
    then msgOrientation . updateCoordinate $ w
    else msgOrientation w
    where
        msgOrientation z = worldMessage (MkMessage { msgType = MovedMsg direction, msgTo = objid}) z
        updateCoordinate = layer %~ A.adjustR (coordinateMove direction) objid
        currentCoordinate = S.elemAt 0 $ A.lookupR objid (w ^. layer)
        newCoordinate = coordinateMove direction currentCoordinate
