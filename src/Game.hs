{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game
    ( Game
    , GameState
    , gameCamera
    , gameKeyAlt
    , gameKeyShift
    , gamePlayer
    , gameRotate
    , gameMove
    , gameAdd
    , gameMsg
    , gameLoad
    , gameShips
    , gameUi
    , runGame
    ) where

import Control.Lens
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import Data.Maybe

import Coordinate
import Message
import Camera
import Link
import Object as O
import Ship
import Ui

-- | Contains the state of the engine (things that will change over time)
data GameState = MkGameState
    { _gameCamera   :: Camera
    , _gameKeyAlt   :: Bool
    , _gameKeyShift :: Bool
    , _gamePlayer   :: Link O.Object
    , _gameShips    :: [Link Ship]
    , _gameUi       :: Ui
    }

newtype Game a = Game { runGame :: MaybeT (StateT GameState IO) a }
    deriving (Functor, Applicative, Monad, MonadState GameState)

resolveLink :: J.FromJSON a => Link a -> Game a
resolveLink = Game . MaybeT . lift . readLink

-- Lenses
gameCamera   :: Lens' GameState Camera
gameKeyAlt   :: Lens' GameState Bool
gameKeyShift :: Lens' GameState Bool
gamePlayer   :: Lens' GameState (Link O.Object)
gameShips    :: Lens' GameState [Link Ship]
gameUi       :: Lens' GameState Ui
gameCamera   = lens _gameCamera   (\s x -> s { _gameCamera   = x })
gameKeyAlt   = lens _gameKeyAlt   (\s x -> s { _gameKeyAlt   = x })
gameKeyShift = lens _gameKeyShift (\s x -> s { _gameKeyShift = x })
gamePlayer   = lens _gamePlayer   (\s x -> s { _gamePlayer   = x })
gameShips    = lens _gameShips    (\s x -> s { _gameShips    = x })
gameUi       = lens _gameUi       (\s x -> s { _gameUi       = x })

instance J.FromJSON GameState where    
    parseJSON (J.Object o) = do
        player <- o J..: "player"
        ships  <- o J..: "ships"
        return $ MkGameState
            { _gameCamera   = defaultCamera
            , _gameKeyAlt   = False
            , _gameKeyShift = False
            , _gamePlayer   = player
            , _gameShips    = ships
            , _gameUi       = defaultUi
            }
    parseJSON _ = error "Unable to parse Game json"

gameAdd :: Object -> Coordinate -> Game ()
gameAdd _ _ = return ()
{-
gameAdd obj coord = do
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
                     :: [Object]

    let things = fmap findThings <$> zip ships ships :: [(Ship, [Object])]
    let nonEmptyThings = filter ((/= 0) . length . snd) things
    let newShip = defaultShip & shipMass .~ (objMass obj) & shipCoordinate .~ coord

    -- Determine if it's an existing ship or if we should create one
    let newShips = case nonEmptyThings of
                [] -> newShip : ships
                sl -> foldr _ [] sl
                      -- grid insert object, change mass ship
                      -- let (gx, gy) = view (shipCoordinate . coordinates) ship in (head s, x + cx - sx, y + cy - sy)
                      -- let newObj = obj { objShipCoordinate = coordinate gx gy }


    -- Save the modified/new ship & object
    modify $ gameWorld . worldShips .~ newShips
    where
        (x, y) = view coordinates coord
-}

gameMsg :: Maybe (Link Object) -> Maybe (Link Object) -> [Message] -> Game ()
gameMsg _ _ _ = return ()
{-
gameMsg _ _ [] = return ()
gameMsg _ Nothing _ = return () -- Replies to non-objects, e.g. direct calls to gameMove
gameMsg fromObj (Just toObj) (msg:msgs) = do
    case toObj of
        Nothing -> return ()
        Just t  -> do
            -- trace ("Sending msg `" ++ show msg ++ "` to object #" ++ show toOid) $ do
            let (responses, newToObj) = runState (objMsg t msg) t
            modify $ gameWorld . worldObjects %~ M.insert toOid newToObj
            gameMsg (Just toOid) fromOid responses
    gameMsg fromOid (Just toOid) msgs
-}

gameRotate :: Link Object -> Direction -> Game ()
gameRotate _ _ = do
    return ()
{-
    objects <- gets (view $ gameWorld . worldObjects)
    case M.lookup oid objects of
        Nothing -> return ()
        Just obj -> do
            let objRotated = obj { objFacing = direction }
            modify $ gameWorld . worldObjects %~ M.insert oid objRotated
            gameMsg Nothing (Just oid) [RotatedMsg direction]
-}

gameMove :: Link Object -> Direction -> Game ()
gameMove resObj direction = do
    -- Rotate the object
    gameRotate resObj direction

{-
    objects <- gets (view $ gameWorld . worldObjects)
    ships   <- gets (view $ gameWorld . worldShips)

    -- Move the object
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
-}

gameLoad :: String -> IO GameState
gameLoad name = do 
    json <- BL.readFile $ "data/" ++ name ++ "/" ++ "game.json"
    return $ fromJust $ J.decode json
