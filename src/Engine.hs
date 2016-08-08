module Engine
    ( engineHandleEvent
    , engineHandleKeyboardEvent
    , engineInit
    , engineLoadGame
    ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State as S
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Linear (V2(V2))
import SDL

import Camera
import Coordinate
import Environment
import Game
import qualified Grid as G
import Link
import Object
import Ship
import Ui
import Ui.Menu

-- TODO: that looks way too disgutsting for what it does
engineInit :: GameState -> ReaderT Environment IO GameState
engineInit game = do
    -- let playerCoord = sysWorldCoordObjectId (view gameWorld newGame) (view gamePlayer newGame)
    let playerCoord = Nothing
    return $ fromMaybe game ((\coord -> game & gameCamera %~ cameraCenter coord) <$> playerCoord)

-- | This function takes care of all events in the engine and dispatches them to the appropriate handlers.
engineHandleEvent :: Event -> Game Bool
engineHandleEvent event = do
    case eventPayload event of
        KeyboardEvent d      -> engineHandleKeyboardEvent d
        WindowResizedEvent d -> engineHandleWindowResizedEvent d
        QuitEvent            -> return True
        _                    -> return False

engineHandleWindowResizedEvent :: WindowResizedEventData -> Game Bool
engineHandleWindowResizedEvent wred = do
    tileSize <- gameEnv envTileSize
    let V2 width height = windowResizedEventSize wred

    modify $ gameCamera . cameraWindowSize .~ V2 (fromIntegral width) (fromIntegral height)
    modify $ gameCamera . cameraViewport .~ V2
        (fromIntegral width `div` fromIntegral tileSize)
        (fromIntegral height `div` fromIntegral tileSize)

    return False

-- | This function handles keyboard events in the engine
engineHandleKeyboardEvent :: KeyboardEventData -> Game Bool
engineHandleKeyboardEvent ked = do
    -- Modifier keys
    case keycode of
        KeycodeLShift -> modify $ gameKeyShift .~ (keymotion == Pressed)    
        KeycodeRShift -> modify $ gameKeyShift .~ (keymotion == Pressed)    
        KeycodeLAlt   -> modify $ gameKeyAlt   .~ (keymotion == Pressed)    
        KeycodeRAlt   -> modify $ gameKeyAlt   .~ (keymotion == Pressed)    
        _             -> return ()

    -- Bare keys
    if (keymotion == Pressed) then do
        (newKeycode, shouldHalt) <- uiMenuInterceptKeycode keycode
        if shouldHalt
        then return True
        else engineHandleBareKeycode newKeycode
    else 
        return False -- $ scancode == ScancodeEscape
    where
        keymotion   = keyboardEventKeyMotion ked -- ^ Wether the key is being pressed or released
        keysym      = keyboardEventKeysym ked    -- ^ Key symbol information: keycode or scancode representation
        keycode     = keysymKeycode keysym       -- ^ Which character is received from the operating system
        -- scancode    = keysymScancode keysym      -- ^ Physical key location as it would be on a US QWERTY keyboard

engineHandleBareKeycode :: Keycode -> Game Bool
engineHandleBareKeycode keycode = do
    player <- gets $ view gamePlayer
    shift  <- gets $ view gameKeyShift
    case keycode of
        KeycodeW       -> if shift then engineRotateObject player North else engineMoveObject player North
        KeycodeS       -> if shift then engineRotateObject player South else engineMoveObject player South
        KeycodeA       -> if shift then engineRotateObject player West  else engineMoveObject player West
        KeycodeD       -> if shift then engineRotateObject player East  else engineMoveObject player East
        -- KeycodeKPPlus  -> modify $ gameCamera %~ cameraZoom (subtract 1)
        -- KeycodeKPMinus -> modify $ gameCamera %~ cameraZoom (+1)
        KeycodeUp      -> modify $ gameCamera %~ cameraMove North
        KeycodeDown    -> modify $ gameCamera %~ cameraMove South
        KeycodeRight   -> modify $ gameCamera %~ cameraMove East
        KeycodeLeft    -> modify $ gameCamera %~ cameraMove West
        KeycodeY       -> modify $ id -- gameCamera %~ (fromMaybe id (cameraTogglePinned <$> sysWorldCoordObjectId world player)) -- TODO: eeeww
        KeycodeE       -> modify $ gameUi       %~ uiMenuSwitch UiMenuMain
        KeycodeEscape  -> modify $ gameUi       %~ uiMenuClear
        _              -> modify $ id
    return False

--engineAddObject :: Object -> Coordinate -> Game ()
--engineAddObject _ _ = return ()
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

--engineMsg :: Maybe (Link Object) -> Maybe (Link Object) -> [Message] -> Game ()
--engineMsg _ _ _ = return ()
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

engineRotateObject :: Link Object -> Direction -> Game ()
engineRotateObject objLink direction = do
    gameModifyLink objLink $ objFacing .~ direction
    -- TODO: Tell the object it got rotated?

engineMoveObject :: Link Object -> Direction -> Game ()
engineMoveObject objLink direction = do
    -- Rotate the object
    engineRotateObject objLink direction
    
    -- Obtaining ship from object
    shipLink <- view objShip <$> gameReadLink objLink
    ship <- gameReadLink shipLink

    -- Asking the ship's grid about the current position of our object
    let maybePosition = G.reverseLookup objLink (view shipGrid ship)
    case maybePosition of
        Nothing -> return ()
        Just position@(x, y) -> do
            let (newX, newY) = coordinatesMove direction position
            gameModifyLink shipLink $ shipGrid %~ G.insert newX newY objLink . G.delete x y objLink

    -- TODO: Add collision detection
    -- Notify the object that its been rotated?

engineLoadGame :: String -> IO GameState
engineLoadGame name = do 
    json <- BL.readFile $ "data/" ++ name ++ "/" ++ "game.json"
    return $ fromJust $ J.decode json
