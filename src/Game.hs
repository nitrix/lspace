module Game where

import SDL.Event
import SDL.Input.Keyboard
import Linear (V2(V2))
import Linear.Affine (Point(P))
import Control.Monad.State (State, modify)
import Control.Lens

import Camera

data GameState = MkGameState { _playerPosition :: Point V2 Integer
                             , _camera :: Camera
                             , _counter :: Int }

camera :: Lens' GameState Camera
camera f s = (\x -> s { _camera = x }) <$> (f $ _camera s)

counter :: Lens' GameState Int
counter f s = (\x -> s { _counter = x }) <$> (f $ _counter s)
                          
gameHandleEvent :: Event -> State GameState Bool
gameHandleEvent event = do
    modify $ counter %~ (+1)

    case eventPayload event of
        QuitEvent -> return True
        KeyboardEvent ked -> do
            let keysym = keyboardEventKeysym ked
            let keymotion = keyboardEventKeyMotion ked
            let keycode = keysymKeycode keysym

            if keymotion == Pressed then
                case keycode of
                    KeycodeUp -> (modify $ camera %~ cameraMoveDown) >> return False
                    KeycodeDown -> (modify $ camera %~ cameraMoveUp) >> return False
                    KeycodeRight -> (modify $ camera %~ cameraMoveLeft) >> return False
                    KeycodeLeft -> (modify $ camera %~ cameraMoveRight) >> return False
                    _ -> case keysymScancode keysym of 
                            ScancodeEscape -> return True
                            _ -> return False
            else
                return False
        _ -> return False

defaultGameState :: GameState
defaultGameState = MkGameState { _playerPosition = P $ V2 0 0
                               , _counter = 0
                               , _camera = defaultCamera
                               }
