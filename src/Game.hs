module Game where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe

import Camera
import qualified Grid as G
import Types.Coordinate
import Types.Game
import Types.Id
import Types.Message
import Types.Object (Object(..))
import Types.Ship
import Types.World

gameAdd :: Object -> Coordinate -> State Game ()
gameAdd obj coord = do
    nextObjId  <- gets $ view $ gameWorld . worldNextObjId
    nextShipId <- gets $ view $ gameWorld . worldNextShipId
    (cx, cy)   <- gets $ view $ gameCamera . cameraCoordinate . coordinates
    ships      <- gets $ view $ gameWorld . worldShips

    -- Find things at the given location
    let findThings s = G.lookup (x + cx - view (shipCoordinate . coordinateX) s)
                                (y + cy - view (shipCoordinate . coordinateY) s) (view shipGrid s) ++
                       G.lookup (x + 1 + cx - view (shipCoordinate . coordinateX) s)
                                (y + cy - view (shipCoordinate . coordinateY) s) (view shipGrid s) ++
                       G.lookup (x + 1 + cx - view (shipCoordinate . coordinateX) s)
                                (y + cy - view (shipCoordinate . coordinateY) s) (view shipGrid s) ++
                       G.lookup (x + cx - view (shipCoordinate . coordinateX) s)
                                (y + 1 + cy - view (shipCoordinate . coordinateY) s) (view shipGrid s) ++
                       G.lookup (x + cx - view (shipCoordinate . coordinateX) s)
                                (y + 1 + cy - view (shipCoordinate . coordinateY) s) (view shipGrid s)
                     :: [ObjectId]

    let things = fmap findThings <$> M.toList ships :: [(ShipId, [ObjectId])]
    let nonEmptyThings = map fst $ filter ((/= 0) . length . snd) things

    -- Determine if it's an existing ship or if we should create one
    let (sid, gx, gy) = case nonEmptyThings of
                [] -> (nextShipId, 0, 0)
                s  -> let (sx, sy) = view (shipCoordinate . coordinates) $ fromJust $ M.lookup (head s) ships
                      in (head s, x + cx - sx, y + cy - sy)

    let newShip = defaultShip & shipMass .~ (objMass obj) & shipCoordinate .~ coord

    -- Set the next object id
    modify $ gameWorld . worldNextObjId +~ 1
    -- Save the object
    modify $ gameWorld . worldObjects %~ M.insert nextObjId (obj { objId = nextObjId, objShipCoordinate = coordinate gx gy })
    -- Save the modified/new ship
    modify $ gameWorld . worldShips %~ M.insertWith
        (\_ old -> old & shipGrid %~ G.insert gx gy nextObjId & shipMass %~ (+) (objMass obj)) sid newShip
    where
        (x, y) = view coordinates coord

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
                let (x, y)       = objShipCoordinate obj ^. coordinates
                let (newX, newY) = coordinateMove direction (objShipCoordinate obj) ^. coordinates

                -- Calculate what the new grid and ship would look like
                let newGrid = G.insert newX newY oid $ G.delete x y oid (ship ^. shipGrid)
                let newShip = shipGrid .~ newGrid $ ship
                let newObj  = obj { objShipCoordinate = coordinate newX newY }

                -- Update the object while also paying attention to collisions
                let objsAtNewLocation = catMaybes $ flip M.lookup objects <$> G.lookup newX newY (ship ^. shipGrid)
                when (not $ any objSolid objsAtNewLocation) $ do
                    modify $ gameWorld . worldShips   %~ M.insert (objShipId newObj) newShip
                    modify $ gameWorld . worldObjects %~ M.insert oid newObj
                    gameMsg Nothing (Just oid) [MovedMsg direction]
