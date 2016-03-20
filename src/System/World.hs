module System.World where

import qualified Assoc as A
import Camera
import Coordinate
import Control.Lens
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Game
import Linear (V2(V2), _x, _y)
import Linear.Affine (Point(P))
import Message
import Object
import World

sysWorldCoordObjectId :: World -> ObjectId -> Maybe Coordinate
sysWorldCoordObjectId w objid = if S.size result > 0 then Just (S.elemAt 0 result) else Nothing
    where
        result = A.lookupR objid (view worldLayer w)

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
    if (fromMaybe False $ all (==False) <$> map objSolid <$> sysWorldObjectsAt w <$> maybeNewCoordinate)
    then msgOrientation . updateCoordinate $ w
    else msgOrientation w
    where
        msgOrientation z   = sysWorldMessage Nothing (Just objid) (MovedMsg direction) z
        updateCoordinate   = worldLayer %~ A.adjustR (coordinateMove direction) objid
        maybeNewCoordinate = coordinateMove direction <$> sysWorldCoordObjectId w objid

-- TODO: Should be named and moved into a System.Camera module... maybe sysCameraBoundedPlayer
-- Also, this needs a serious rewriting
sysWorldMovePlayer :: ObjectId -> Direction -> Game -> Game
sysWorldMovePlayer player direction game = newGame & gameCamera %~ fixCamera
    where
        world = view gameWorld newGame
        newGame = game & gameWorld %~ sysWorldMoveObject direction player
        fixCamera = fromMaybe id (focus <$> sysWorldCoordObjectId world player)
        focus (P (V2 x y)) camera = camera &~ do
            cameraCoordinate .= coordinate (min minCameraX (x-padding)) (min minCameraY (y-padding))
            cameraCoordinate %= \(P (V2 cx cy)) -> if x >= cx+maxCameraX-1-padding then coordinate (x-maxCameraX+1+padding) cy else coordinate cx cy
            cameraCoordinate %= \(P (V2 cx cy)) -> if y >= cy+maxCameraY-1-padding then coordinate cx (y-maxCameraY+1+padding) else coordinate cx cy
        padding    = min (maxCameraX `div` 4) (maxCameraY `div` 4)
        minCameraX = view (gameCamera . cameraCoordinate . coordinateX) newGame
        minCameraY = view (gameCamera . cameraCoordinate . coordinateY) newGame
        maxCameraX = (!!zoomLevel) $ iterate (*2) $ toInteger $ view (gameCamera . cameraViewport . _x) newGame
        maxCameraY = (!!zoomLevel) $ iterate (*2) $ toInteger $ view (gameCamera . cameraViewport . _y) newGame
        zoomLevel  = game ^. gameCamera . cameraZoomLevel

sysWorldAddObjectAtPlayer :: Object -> State Game ()
sysWorldAddObjectAtPlayer obj = do
    -- Increment world next object id
    -- modify $ gameWorld . worldNextObjectId +~ 1
    objid <- gets $ succ . fst . M.findMax . view (gameWorld . worldObjects)

    -- Add our object to worldObjects
    modify $ gameWorld . worldObjects %~ M.insert objid obj

    -- Add our object to worldLayer
    player <- gets $ view gamePlayer
    world  <- gets $ view gameWorld 
    modify $ gameWorld . worldLayer %~ fromMaybe id ((\coord -> A.insert coord objid) <$> sysWorldCoordObjectId world player)
