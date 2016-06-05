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

-- import Debug.Trace

gameMsg :: Maybe ObjectId -> Maybe ObjectId -> [Message] -> State Game ()
gameMsg _ _ [] = return ()
gameMsg _ Nothing _ = return () -- Replies to non-objects, e.g. direct calls to gameMove
gameMsg fromOid (Just toOid) (msg:msgs) = do
    objects <- gets (view $ gameWorld . worldObjects)
    case M.lookup toOid objects of
        Nothing    -> return ()
        Just toObj -> do
            -- trace ("Sending msg `" ++ show msg ++ "` to object #" ++ show toOid) $ do
            let (responses, newToObj) = runState (objMsg toObj msg) toObj
            modify $ gameWorld . worldObjects %~ M.insert toOid newToObj
            gameMsg (Just toOid) fromOid responses
    gameMsg fromOid (Just toOid) msgs

gameRotate :: ObjectId -> Direction -> State Game ()
gameRotate oid direction = do
    objects <- gets (view $ gameWorld . worldObjects)
    case M.lookup oid objects of
        Nothing -> return ()
        Just obj -> do
            let objRotated = obj { objFacing = direction }
            modify $ gameWorld . worldObjects %~ M.insert oid objRotated
            gameMsg Nothing (Just oid) [RotatedMsg direction]

gameMove :: ObjectId -> Direction -> State Game ()
gameMove oid direction = do
    -- Rotate the object
    gameRotate oid direction

    -- Move the object
    objects <- gets (view $ gameWorld . worldObjects)
    ships   <- gets (view $ gameWorld . worldShips)
    case M.lookup oid objects of
        Nothing  -> return ()
        Just obj -> case M.lookup (objShipId obj) ships of
            Nothing -> return ()
            Just ship -> do
                -- The most important, the previous and new coordinates
                let (x, y)       = objCoordinate obj ^. coordinates
                let (newX, newY) = coordinateMove direction (objCoordinate obj) ^. coordinates

                -- Calculate what the new grid and ship would look like
                let newGrid = G.insert newX newY oid $ G.delete x y oid (ship ^. shipGrid)
                let newShip = shipGrid .~ newGrid $ ship
                let newObj  = obj { objCoordinate = coordinate newX newY }

                -- Update the object while also paying attention to collisions
                let objsAtNewLocation = catMaybes $ flip M.lookup objects <$> G.lookup newX newY (ship ^. shipGrid)
                when (not $ any objSolid objsAtNewLocation) $ do
                    modify $ gameWorld . worldShips   %~ M.insert (objShipId newObj) newShip
                    modify $ gameWorld . worldObjects %~ M.insert oid newObj
                    gameMsg Nothing (Just oid) [MovedMsg direction]
