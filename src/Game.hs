module Game where

import SDL.Event (Event, eventPayload, EventPayload(QuitEvent))
import Linear (V2(V2), V4(V4))
import Linear.Affine (Point(P))
import Control.Monad.State (State, get)
import Debug.Trace (traceM)

data GameState = MkGameState { playerPosition :: Point V2 Integer
                             , counter :: Int }

gameDefaultState :: GameState
gameDefaultState = MkGameState { playerPosition = P $ V2 0 0
                               , counter = 0
                               }

gameHandleEvent :: Event -> State GameState Bool
gameHandleEvent event = do
    let quit = eventPayload event == QuitEvent
    return quit