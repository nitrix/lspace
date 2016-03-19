module Engine
    ( engineHandleEvent
    , engineHandleKeyboardEvent
    ) where

import Camera
import Coordinate
import Control.Lens
import Control.Monad.State as S
import Game
import Message
import SDL
import System.World
import Ui
import Ui.Menu

-- | This function takes care of all events in the engine and dispatches them to the appropriate handlers.
engineHandleEvent :: Event -> State Game Bool
engineHandleEvent event =
    case eventPayload event of
        KeyboardEvent ked -> engineHandleKeyboardEvent ked
        QuitEvent         -> return True
        _                 -> return False

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

    case keycode of
        KeycodeUp     -> modify $ gameCamera %~ cameraMove UpDirection
        KeycodeDown   -> modify $ gameCamera %~ cameraMove DownDirection
        KeycodeRight  -> modify $ gameCamera %~ cameraMove RightDirection
        KeycodeLeft   -> modify $ gameCamera %~ cameraMove LeftDirection
        KeycodeW      -> modify $ gameWorld  %~ sysWorldMoveObject UpDirection    player
        KeycodeB      -> modify $ gameWorld  %~ sysWorldMoveObject UpDirection    player
        KeycodeS      -> modify $ gameWorld  %~ sysWorldMoveObject DownDirection  player
        KeycodeA      -> modify $ gameWorld  %~ sysWorldMoveObject LeftDirection  player
        KeycodeD      -> modify $ gameWorld  %~ sysWorldMoveObject RightDirection player
        KeycodeR      -> modify $ gameWorld  %~ sysWorldMessage Nothing (Just player) RotateMsg
        KeycodeE      -> modify $ gameUi     %~ uiMenuSwitch UiMenuMain
        KeycodeEscape -> modify $ gameUi     %~ uiMenuClear
        _             -> modify $ id

    return False
