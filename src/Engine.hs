module Engine
    ( engineHandleEvent
    , engineHandleKeyboardEvent
    , engineInit
    , enginePokeIO
    ) where

import Camera
import Coordinate
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State as S
import Demo
import Data.Maybe
import Environment
import Game
import Linear (V2(V2))
import Message
import SDL
import System.World
import Ui
import Ui.Menu
import World

enginePokeIO :: Game -> EnvironmentT IO Game
enginePokeIO game = do
    window          <- asks envWindow
    tileSize        <- asks envTileSize
    V2 width height <- SDL.get $ windowSize window
    return $ game & gameCamera . cameraViewport .~ V2 (width `div` fromIntegral tileSize) (height `div` fromIntegral tileSize)

engineInit :: Game -> ReaderT Environment IO Game
engineInit game = do
    newGame <- enginePokeIO $ gamePlayer .~ demoPlayer $ game
    return $ fromMaybe newGame ((\coord -> newGame & gameCamera %~ cameraCenter coord) <$> playerCoord)
    where
        world = view gameWorld game &~ do
             worldLayer   .= demoLayer
             worldObjects .= demoObjects
        playerCoord = sysWorldCoordObjectId world $ view gamePlayer game

-- | This function takes care of all events in the engine and dispatches them to the appropriate handlers.
engineHandleEvent :: Event -> State Game Bool
engineHandleEvent event =
    case eventPayload event of
        KeyboardEvent ked           -> engineHandleKeyboardEvent ked
        QuitEvent                   -> return True
        _                           -> return False

-- | This function handles keyboard events in the engine
engineHandleKeyboardEvent :: KeyboardEventData -> State Game Bool
engineHandleKeyboardEvent ked = do
    if (keymotion == Pressed) then do
        (newKeycode, shouldHalt) <- uiMenuInterceptKeycode keycode
        if shouldHalt then return True else engineHandleBareKeycode newKeycode
    else 
        return False -- $ scancode == ScancodeEscape
    where
        keymotion   = keyboardEventKeyMotion ked -- ^ Wether the key is being pressed or released
        keysym      = keyboardEventKeysym ked    -- ^ Key symbol information: keycode or scancode representation
        keycode     = keysymKeycode keysym       -- ^ Which character is received from the operating system
        -- scancode    = keysymScancode keysym      -- ^ Physical key location as it would be on a US QWERTY keyboard

engineHandleBareKeycode :: Keycode -> State Game Bool
engineHandleBareKeycode keycode = do
    player <- gets $ view gamePlayer
    world  <- gets $ view gameWorld

    case keycode of
        KeycodeW       -> modify $ sysWorldMovePlayer player UpDirection
        KeycodeB       -> modify $ sysWorldMovePlayer player UpDirection
        KeycodeS       -> modify $ sysWorldMovePlayer player DownDirection
        KeycodeA       -> modify $ sysWorldMovePlayer player LeftDirection
        KeycodeD       -> modify $ sysWorldMovePlayer player RightDirection
        KeycodeKPPlus  -> modify $ gameCamera %~ cameraZoom (subtract 1)
        KeycodeKPMinus -> modify $ gameCamera %~ cameraZoom (+1)
        KeycodeUp      -> modify $ gameCamera %~ cameraMove UpDirection
        KeycodeDown    -> modify $ gameCamera %~ cameraMove DownDirection
        KeycodeRight   -> modify $ gameCamera %~ cameraMove RightDirection
        KeycodeLeft    -> modify $ gameCamera %~ cameraMove LeftDirection
        KeycodeY       -> modify $ gameCamera %~ (fromMaybe id (cameraTogglePinned <$> sysWorldCoordObjectId world player)) -- TODO: eeeww
        KeycodeR       -> modify $ gameWorld  %~ sysWorldMessage Nothing (Just player) RotateMsg
        KeycodeE       -> modify $ gameUi     %~ uiMenuSwitch UiMenuMain
        KeycodeEscape  -> modify $ gameUi     %~ uiMenuClear
        _              -> modify $ id

    return False
