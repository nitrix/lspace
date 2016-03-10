module World
    ( World(..)
    , WorldLayer
    , WorldObjects
    , defaultWorld
    , worldLayer
    , worldMoveObject
    , worldObjects
    , worldObjectsAt
) where

import qualified Assoc as A
import Coordinate
import Control.Lens
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import Object
import System.Message

type WorldLayer = A.Assoc Coordinate ObjectId
type WorldObjects = M.Map ObjectId Object

data World = MkWorld
    { _worldLayer   :: WorldLayer -- TODO: multiple layers
    , _worldObjects :: WorldObjects
    }

-- Lenses
worldLayer   :: Lens' World WorldLayer
worldObjects :: Lens' World WorldObjects
worldLayer   = lens _worldLayer   (\s x -> s { _worldLayer = x })
worldObjects = lens _worldObjects (\s x -> s { _worldObjects = x })

-- Empty world
defaultWorld :: World
defaultWorld = MkWorld
    { _worldLayer   = A.empty
    , _worldObjects = M.empty
    }

-- | Gives the list of objects at a given world coordinate (regardless of their layer)
worldObjectsAt :: World -> Coordinate -> [Object]
worldObjectsAt w c = mapMaybe resolveObjectIds objectIds
    where
        resolveObjectIds :: ObjectId -> Maybe Object
        resolveObjectIds oid = M.lookup oid $ view worldObjects w
        objectIds :: [ObjectId]
        objectIds = S.toList $ A.lookup c $ view worldLayer w

-- | Sends a message. The origin and the destination can both be either an object (Just) or a system (Nothing).
worldMessage :: Maybe ObjectId -> Maybe ObjectId -> Message -> World -> World
worldMessage fromObjId Nothing        = worldMessageSystem fromObjId
worldMessage fromObjId (Just toObjId) = worldMessageObject fromObjId toObjId

-- | Sends a message to an object. The origin can either be another object (Just) or a system (Nothing).
worldMessageObject :: Maybe ObjectId -> ObjectId -> Message -> World -> World
worldMessageObject fromObjId toObjId m w =
    trace ("From object id: " ++ show fromObjId) $
    trace ("To object id: " ++ show toObjId) $
    trace ("Message: " ++ show m) $
    case response of
        Just (newMsgs, newObj) -> foldr (worldMessage (Just toObjId) fromObjId)
                                        (worldObjects %~ M.insert toObjId newObj $ w)
                                        newMsgs
        Nothing                -> w
    where
        response :: Maybe ([Message], Object)
        response = flip objMsg m <$> M.lookup toObjId (view worldObjects w)

-- | Sends a message to a system. The origin can either be an object (Just) or another system (Nothing).
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
        updateCoordinate = worldLayer %~ A.adjustR (coordinateMove direction) objid
        currentCoordinate = S.elemAt 0 $ A.lookupR objid (view worldLayer w)
        newCoordinate = coordinateMove direction currentCoordinate
