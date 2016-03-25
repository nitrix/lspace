module Game
    ( Game
    , defaultGame
    , gameCamera
    , gamePlayer
    , gameUi
    , gameWorld
    ) where

import Camera
import Control.Lens
import Demo
import Object
import Ui
import World

-- | Contains the state of the engine (things that will change over time)
data Game = MkGame
    { _gamePlayer  :: ObjectId
    , _gameCamera  :: Camera
    , _gameWorld   :: World
    , _gameUi      :: Ui
    }

-- Lenses
gameCamera :: Lens' Game Camera
gameWorld  :: Lens' Game World
gamePlayer :: Lens' Game ObjectId
gameUi     :: Lens' Game Ui
gameCamera = lens _gameCamera (\s x -> s { _gameCamera = x })
gameWorld  = lens _gameWorld  (\s x -> s { _gameWorld  = x })
gamePlayer = lens _gamePlayer (\s x -> s { _gamePlayer = x })
gameUi     = lens _gameUi     (\s x -> s { _gameUi = x })

-- | Default engine state with an empty world, player and camera at 0,0
defaultGame :: Game
defaultGame = MkGame
    { _gameCamera  = defaultCamera
    , _gamePlayer  = 0
    , _gameUi      = defaultUi
    , _gameWorld   = defaultWorld &~ do
                         worldLayer   .= demoLayer
                         worldObjects .= demoObjects
    }
