module Engine
    ( engineHandleEvent
    , engineHandleKeyboardEvent
    , engineInit
    ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State as S
import Data.List
import Data.Maybe
import Debug.Trace
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

engineAddObject :: Link Object -> Coordinate -> Game ()
engineAddObject objLink coord = do
    nearbyObjectLinks <- concat <$> mapM engineObjectsAtLocation
        [ coordinateMove North coord
        , coordinateMove East coord
        , coordinateMove South coord
        , coordinateMove West coord
        ]

    nearbyUniqueShipLinks <- nub <$> (fmap (view objShip) <$> mapM gameReadLink nearbyObjectLinks)
    
    let (worldX, worldY) = view coordinates coord
    
    case nearbyUniqueShipLinks of
        [] -> do
            trace "Nothing around" $ do
            let ship = defaultShip & shipCoordinate .~ coord
            newShipLink <- gameCreateLink ship
            gameModifyLink objLink $ objShip .~ newShipLink
            gameModifyLink newShipLink $ shipGrid %~ G.insert 0 0 objLink
            modify $ gameShips %~ (newShipLink:)
        x:[] -> do
            trace "One thing around" $ do
            s <- gameReadLink x
            let (innerX, innerY) = (worldX - s ^. shipCoordinate . coordinateX, worldY - s ^. shipCoordinate . coordinateY)
            gameModifyLink objLink $ objShip .~ x
            gameModifyLink x $ shipGrid %~ G.insert innerX innerY objLink
        x:xs -> do
            trace "Many around" $ do
            return () -- TODO
    
    return ()

engineRemoveObject :: Link Object -> Game ()
engineRemoveObject objLink = do
    shipLink <- view objShip <$> gameReadLink objLink
    gameModifyLink shipLink $ shipGrid %~ G.reverseDelete objLink

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

engineRotateObject :: Link Object -> Direction -> Game ()
engineRotateObject objLink direction = do
    gameModifyLink objLink $ objFacing .~ direction

engineObjectsAtLocation :: Coordinate -> Game [Link Object]
engineObjectsAtLocation coord = do
    shipLinks   <- gets (view gameShips)
    ships       <- mapM gameReadLink shipLinks
    
    return $ concat <$> forM ships $ \s -> do
        let (innerX, innerY) = (worldX - s ^. shipCoordinate . coordinateX, worldY - s ^. shipCoordinate . coordinateY)
        G.lookup innerX innerY $ view shipGrid s
        
    where
        (worldX, worldY) = view coordinates coord

-- Unless Ship is broken or we're looking at the wrong ship for our object,
-- it should always be able to get the coordinate of the object.
-- Thus, I decided to reflect this in the type and provide defaultCoordinate as an absolute emergency.
-- It simplifies code that uses engineObjectLocation a whole lot.
engineObjectLocation :: Link Object -> Game Coordinate
engineObjectLocation objLink = do
    shipLink <- view objShip <$> gameReadLink objLink
    ship     <- gameReadLink shipLink
    
    let (x, y) = view coordinates $ fromMaybe defaultCoordinate
                                  $ uncurry coordinate <$> G.reverseLookup objLink (view shipGrid ship)
                                  
    return $ coordinate (ship ^. shipCoordinate . coordinateX + x)
                        (ship ^. shipCoordinate . coordinateY + y)
            
engineMoveObject :: Link Object -> Direction -> Game ()
engineMoveObject objLink direction = do
    trace "engineMoveObject" $ do
    
    -- Rotate the object
    engineRotateObject objLink direction
    
    trace "Moar" $ do

    -- Asking the ship's grid about the current position of our object
    newLocation <- coordinateMove direction <$> engineObjectLocation objLink
    
    trace "Even moar" $ do
    
    traceShow newLocation $ do
    
    -- We're going to perform collision detection of native objects at the target location
    natives <- mapM gameReadLink =<< engineObjectsAtLocation newLocation
    
    trace "Too moar" $ do
    
    -- Allow moving the object only when there's no collisions detected
    when (all (not . objSolid) natives) $ do
        engineRemoveObject objLink
        engineAddObject objLink newLocation