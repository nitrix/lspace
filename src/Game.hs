module Game where

import SDL.Event
import SDL.Input.Keyboard
import Linear (V2(V2))
import Linear.Affine (Point(P))
import Control.Monad.State (State, modify)
import Control.Lens

import Camera

data GameState = MkGameState { playerPosition :: Point V2 Integer
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
            case (keysymKeycode $ keyboardEventKeysym ked) of
                KeycodeUp -> modify $ camera %~ cameraMoveUp
                KeycodeDown -> modify $ camera %~ cameraMoveDown
                KeycodeRight -> modify $ camera %~ cameraMoveRight
                KeycodeLeft -> modify $ camera %~ cameraMoveLeft
                _ -> return ()
            return False
        _ -> return False

gameDefaultState :: GameState
gameDefaultState = MkGameState { playerPosition = P $ V2 0 0
                               , _counter = 0
                               , _camera = defaultCamera
                               }
