module System.World where

import qualified Assoc as A
import Coordinate
import Control.Lens
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Game
import Message
import Object
import World

coordinateObjectId :: World -> ObjectId -> Coordinate
coordinateObjectId w objid = S.elemAt 0 $ A.lookupR objid (view worldLayer w)

sysWorldAddObjectAtPlayer :: Object -> State Game ()
sysWorldAddObjectAtPlayer obj = do
    player <- gets $ view gamePlayer
    return ()
    
-- | Sends a message. The origin and the destination can both be either an object (Just) or a system (Nothing).
sysWorldMessage :: Maybe ObjectId -> Maybe ObjectId -> Message -> World -> World
sysWorldMessage _ Nothing _ w = 
    -- trace ("From object id: " ++ show fromObjId) $
    -- trace ("To: System") $
    -- trace ("Message: " ++ show m) $
    w
sysWorldMessage fromObjId (Just toObjId) m w = 
    -- trace ("From object id: " ++ show fromObjId) $
    -- trace ("To object id: " ++ show toObjId) $
    -- trace ("Message: " ++ show m) $
    case flip objMsg m <$> M.lookup toObjId (view worldObjects $ w) of
        Just (newMsgs, newObj) ->
            foldr (sysWorldMessage (Just toObjId) fromObjId)
                  (worldObjects %~ M.insert toObjId newObj $ w)
                  newMsgs
        Nothing                -> w

-- | Gives the list of objects at a given world coordinate (regardless of their layer)
sysWorldObjectsAt :: World -> Coordinate -> [Object]
sysWorldObjectsAt w c = mapMaybe resolveObjectIds objectIds
    where
        resolveObjectIds :: ObjectId -> Maybe Object
        resolveObjectIds oid = M.lookup oid $ view worldObjects w
        objectIds :: [ObjectId]
        objectIds = S.toList $ A.lookup c $ view worldLayer w

-- TODO: Needs a serious rewrite eventually; e.g. giving proximity/stepped on events and so on
sysWorldMoveObject :: Direction -> ObjectId -> World -> World
sysWorldMoveObject direction objid w =
    if (all (==False) $ map objSolid $ sysWorldObjectsAt w newCoordinate)
    then msgOrientation . updateCoordinate $ w
    else msgOrientation w
    where
        msgOrientation z  = sysWorldMessage Nothing (Just objid) (MovedMsg direction) z
        updateCoordinate  = worldLayer %~ A.adjustR (coordinateMove direction) objid
        newCoordinate     = coordinateMove direction $ coordinateObjectId w objid