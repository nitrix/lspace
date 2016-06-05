module Game where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe

import qualified Grid as G
import Types.Coordinate
import Types.Game
import Types.Message
import Types.Object (ObjectId, Object(..))
import Types.Ship
import Types.World

import Debug.Trace

gameMsg :: Maybe ObjectId -> Maybe ObjectId -> [Message] -> State Game ()
gameMsg _ _ [] = return ()
gameMsg _ Nothing _ = return () -- Replies to non-objects, e.g. direct calls to gameMove
gameMsg fromOid (Just toOid) (msg:msgs) = do
    objects <- gets (view $ gameWorld . worldObjects)
    case M.lookup toOid objects of
        Nothing    -> return ()
        Just toObj -> do
            trace ("Sending msg `" ++ show msg ++ "` to object #" ++ show toOid) $ do
            trace ("Obj coordinates before: " ++ show (objCoordinate toObj)) $ do
            let (responses, newToObj) = runState (objMsg toObj msg) toObj
            -- let fixNewToObj = newToObj { objCoordinate = objCoordinate toObj }
            let fixNewToObj = newToObj
            trace ("Obj coordinates after: " ++ show (objCoordinate fixNewToObj)) $ do
            modify $ gameWorld . worldObjects %~ M.insert toOid fixNewToObj
            gameMsg (Just toOid) fromOid responses
    gameMsg fromOid (Just toOid) msgs

gameMove :: ObjectId -> Direction -> State Game ()
gameMove oid direction = do
    objects <- gets (view $ gameWorld . worldObjects)
    ships   <- gets (view $ gameWorld . worldShips)
    case M.lookup oid objects of
        Nothing  -> return ()
        Just obj -> case M.lookup (objShipId obj) ships of
            Nothing -> return ()
            Just ship -> do
                let (x, y)       = objCoordinate obj ^. coordinates
                let (newX, newY) = coordinateMove direction (objCoordinate obj) ^. coordinates
                let objsAtNewLocation = catMaybes $ flip M.lookup objects <$> G.lookup newX newY (ship ^. shipGrid)
                if any objSolid objsAtNewLocation
                then return () -- still turn the object facing in that direction
                else do
                    let newGrid = G.insert newX newY oid $ G.delete x y oid (ship ^. shipGrid)
                    let newShip = shipGrid .~ newGrid $ ship
                    let newObj  = obj { objCoordinate = coordinate newX newY
                                      , objFacing     = direction
                                      }
                    modify $ gameWorld . worldShips   %~ M.insert (objShipId newObj) newShip
                    modify $ gameWorld . worldObjects %~ M.insert oid newObj
                    -- Message the object that it moved and rotated
                    gameMsg Nothing (Just oid) [MovedMsg direction, RotatedMsg direction]
                    -- TODO: Messages from `step on` and `proximity`
