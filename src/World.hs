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
import Debug.Trace
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

worldMessage :: Maybe ObjectId -> Maybe ObjectId -> Message -> World -> World
worldMessage fromObjId Nothing        = worldMessageSystem fromObjId
worldMessage fromObjId (Just toObjId) = worldMessageObject fromObjId toObjId

worldMessageObject :: Maybe ObjectId -> ObjectId -> Message -> World -> World
worldMessageObject fromObjId toObjId m w =
    trace ("From object id: " ++ show fromObjId) $
    trace ("To object id: " ++ show toObjId) $
    trace ("Message: " ++ show m) $
    case response of
        Just (newMsgs, newObj) -> foldr (worldMessage (Just toObjId) fromObjId)
                                        (objects %~ M.insert toObjId newObj $ w)
                                        newMsgs
        Nothing                -> w
    where
        response :: Maybe ([Message], Object)
        response = flip objMsg m <$> M.lookup toObjId (_objects w)

worldMessageSystem :: Maybe ObjectId -> Message -> World -> World
worldMessageSystem fromObjId msg w =
    trace ("From object id: " ++ show fromObjId) $
    trace ("To: System") $
    trace ("Message: " ++ show msg) $
    w

-- TODO: Needs a serious rewrite eventually
worldMoveObject :: Direction -> ObjectId -> World -> World
worldMoveObject direction objid w =
    if (all (==False) $ map objSolid $ worldObjectsAt w newCoordinate)
    then msgOrientation . updateCoordinate $ w
    else msgOrientation w
    where
        msgOrientation z = worldMessage Nothing (Just objid) (MovedMsg direction) z
        updateCoordinate = layer %~ A.adjustR (coordinateMove direction) objid
        currentCoordinate = S.elemAt 0 $ A.lookupR objid (w ^. layer)
        newCoordinate = coordinateMove direction currentCoordinate
