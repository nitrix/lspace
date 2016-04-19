module Types.Game
    ( Game
    , defaultGame
    , gameCamera
    , gamePlayer
    -- , gameRenderer
    , gameUi
    , gameWorld
    ) where

import Control.Lens
import Camera
import Types.Object
-- import Types.Renderer
import Types.Ui
import Types.World

-- | Contains the state of the engine (things that will change over time)
data Game = MkGame
    { _gameCamera   :: Camera
    , _gamePlayer   :: ObjectId
    -- , _gameRenderer :: Renderer
    , _gameUi       :: Ui
    , _gameWorld    :: World
    }

-- Lenses
gameCamera   :: Lens' Game Camera
gamePlayer   :: Lens' Game ObjectId
-- gameRenderer :: Lens' Game Renderer
gameUi       :: Lens' Game Ui
gameWorld    :: Lens' Game World
gameCamera   = lens _gameCamera   (\s x -> s { _gameCamera   = x })
gamePlayer   = lens _gamePlayer   (\s x -> s { _gamePlayer   = x })
-- gameRenderer = lens _gameRenderer (\s x -> s { _gameRenderer = x })
gameUi       = lens _gameUi       (\s x -> s { _gameUi       = x })
gameWorld    = lens _gameWorld    (\s x -> s { _gameWorld    = x })

-- | Default engine state with an empty world, player and camera at 0,0
defaultGame :: Game
defaultGame = MkGame
    { _gameCamera   = defaultCamera
    , _gamePlayer   = 0
    -- , _gameRenderer = defaultRenderer
    , _gameUi       = defaultUi
    , _gameWorld    = defaultWorld
    }
