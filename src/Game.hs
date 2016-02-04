module Game where

import SDL.Event
import SDL.Input.Keyboard
import SDL.Input.Keyboard.Codes
import Linear.Affine (Point(P))
import Control.Monad.State (State, modify)
import Linear (V2(V2))

import Camera

data GameState = MkGameState { playerPosition :: Point V2 Integer
                             , camera :: Camera
                             , counter :: Int }
                          
gameDefaultState :: GameState
gameDefaultState = MkGameState { playerPosition = P $ V2 0 0
                               , counter = 0
                               }

gameHandleEvent :: Event -> State GameState Bool
gameHandleEvent event = do
    modifyCounter (+1)

    case eventPayload event of
        QuitEvent -> return True
        KeyboardEvent ked ->
            case (keysymKeycode $ keyboardEventKeysym ked) of
                KeycodeUp -> modifyCamera cameraMoveUp
                KeycodeDown -> modifyCamera cameraMoveDown
                KeycodeRight -> modifyCamera cameraMoveRight
                KeycodeLeft -> modifyCamera cameraMoveLeft
                _ -> return False
                
    where
        modifyCamera :: (Camera -> Camera) -> State GameState Bool
        modifyCamera f = do { modify $ \gs -> gs { camera = f $ camera gs }; return False }
        modifyCounter :: (Int -> Int) -> State GameState Bool
        modifyCounter f = do { modify $ \gs -> gs { counter = f $ counter gs }; return False }
