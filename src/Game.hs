module Game where

import Camera
import Control.Lens
import Control.Monad.State
import Coordinate
import SDL.Event
import SDL.Input.Keyboard
import World

data GameState = MkGameState
    { _playerPosition :: Coordinate
    , _camera         :: Camera
    , _world          :: World
    }

-- TODO: Wait for GHC8 and then switch to makeLenses
camera :: Lens' GameState Camera
camera f s = (\x -> s { _camera = x }) <$> f (_camera s)

-- TODO: Wait for GHC8 and then switch to makeLenses
world :: Lens' GameState World
world f s = (\x -> s { _world = x }) <$> f (_world s)

defaultGameState :: GameState
defaultGameState = MkGameState
    { _playerPosition = defaultCoordinate
    , _camera         = defaultCamera
    , _world          = defaultWorld
    }

-- | This function takes care of all events in the game and dispatches them to the appropriate handlers.
gameHandleEvent :: Event -> State GameState Bool
gameHandleEvent event =
    case eventPayload event of
        KeyboardEvent ked -> gameHandleKeyboardEvent ked
        QuitEvent         -> return True
        _                 -> return False

-- | This function handles keyboard events in the game
gameHandleKeyboardEvent :: KeyboardEventData -> State GameState Bool
gameHandleKeyboardEvent ked =
    if keymotion == Pressed then
        case keycode of
            KeycodeUp    -> modify (camera %~ cameraMoveUp)        >> return False
            KeycodeDown  -> modify (camera %~ cameraMoveDown)      >> return False
            KeycodeRight -> modify (camera %~ cameraMoveRight)     >> return False
            KeycodeLeft  -> modify (camera %~ cameraMoveLeft)      >> return False
            KeycodeT     -> modify (world  %~ worldTestToggleDoor) >> return False
            _            -> case scancode of 
                                ScancodeEscape -> return True
                                _              -> return False
    else
        return False
    where
        keymotion = keyboardEventKeyMotion ked -- ^ Wether the key is being pressed or released
        keysym    = keyboardEventKeysym ked    -- ^ Key symbol information with two representations available: keycode or scancode
        keycode   = keysymKeycode keysym       -- ^ Which character is received from the operating system
        scancode  = keysymScancode keysym      -- ^ Physical key location as it would be on a US QWERTY keyboard
