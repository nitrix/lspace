module Game where

import SDL.Event
import SDL.Input.Keyboard
import Control.Monad.State (State, modify)
import Control.Lens

import Camera
import Coordinate
import World

data GameState = MkGameState { _playerPosition :: Coordinate
                             , _camera :: Camera
                             , _world :: World
                             }

camera :: Lens' GameState Camera
camera f s = (\x -> s { _camera = x }) <$> (f $ _camera s)

world :: Lens' GameState World
world f s = (\x -> s { _world = x }) <$> (f $ _world s)

defaultGameState :: GameState
defaultGameState = MkGameState { _playerPosition = defaultCoordinate
                               , _camera = defaultCamera
                               , _world = defaultWorld
                               }

gameHandleEvent :: Event -> State GameState Bool
gameHandleEvent event = do
    case eventPayload event of
        QuitEvent -> return True
        KeyboardEvent ked -> do
            let keysym = keyboardEventKeysym ked
            let keymotion = keyboardEventKeyMotion ked
            let keycode = keysymKeycode keysym

            if keymotion == Pressed then
                case keycode of
                    KeycodeUp -> (modify $ camera %~ cameraMoveUp) >> return False
                    KeycodeDown -> (modify $ camera %~ cameraMoveDown) >> return False
                    KeycodeRight -> (modify $ camera %~ cameraMoveRight) >> return False
                    KeycodeLeft -> (modify $ camera %~ cameraMoveLeft) >> return False
                    _ -> case keysymScancode keysym of 
                            ScancodeEscape -> return True
                            _ -> return False
            else
                return False
        _ -> return False
