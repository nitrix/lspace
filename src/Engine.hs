module Engine
    ( engineHandleEvent
    , engineHandleKeyboardEvent
    , engineInit
    ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State as S
import Data.Maybe
import Linear (V2(V2))
import SDL

import Camera
import Game
import Types.Coordinate
import Types.Environment
import Types.Game
import Types.Ui
import Ui.Menu

-- TODO: that looks way too disgutsting for what it does
engineInit :: Game -> ReaderT Environment IO Game
engineInit game = do
    -- let playerCoord = sysWorldCoordObjectId (view gameWorld newGame) (view gamePlayer newGame)
    let playerCoord = Nothing
    return $ fromMaybe game ((\coord -> game & gameCamera %~ cameraCenter coord) <$> playerCoord)

-- TODO: disgusting environment passed explicitly
-- | This function takes care of all events in the engine and dispatches them to the appropriate handlers.
engineHandleEvent :: Environment -> Event -> StateT Game IO Bool
engineHandleEvent env event = do
    case eventPayload event of
        KeyboardEvent d      -> state $ runState (engineHandleKeyboardEvent d)
        WindowResizedEvent d -> engineHandleWindowResizedEvent env d
        QuitEvent            -> return True
        _                    -> return False

-- TODO: disgusting environment passed explicitly
engineHandleWindowResizedEvent :: Environment -> WindowResizedEventData -> StateT Game IO Bool
engineHandleWindowResizedEvent env wred = do
    ws <- SDL.get $ windowSize $ envWindow env

    let tileSize = envTileSize env
    let V2 width height = windowResizedEventSize wred

    modify $ gameCamera . cameraWindowSize .~ ws
    modify $ gameCamera . cameraViewport .~ V2
        (fromIntegral width `div` fromIntegral tileSize)
        (fromIntegral height `div` fromIntegral tileSize)

    return False

-- | This function handles keyboard events in the engine
engineHandleKeyboardEvent :: KeyboardEventData -> State Game Bool
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

engineHandleBareKeycode :: Keycode -> State Game Bool
engineHandleBareKeycode keycode = do
    player <- gets $ view gamePlayer
    shift  <- gets $ view gameKeyShift
    case keycode of
        KeycodeW       -> if shift then gameRotate player North else gameMove player North
        KeycodeS       -> if shift then gameRotate player South else gameMove player South
        KeycodeA       -> if shift then gameRotate player West  else gameMove player West
        KeycodeD       -> if shift then gameRotate player East  else gameMove player East
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
