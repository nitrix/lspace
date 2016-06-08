module Types.Game
    ( Game
    , defaultGame
    , gameCamera
    , gameKeyAlt
    , gameKeyShift
    , gamePlayer
    , gameUi
    , gameWorld
    ) where

import Control.Lens

import Camera
import Types.Id
import Types.Ui
import Types.World

-- | Contains the state of the engine (things that will change over time)
data Game = MkGame
    { _gameCamera   :: Camera
    , _gameKeyAlt   :: Bool
    , _gameKeyShift :: Bool
    , _gamePlayer   :: ObjectId
    , _gameUi       :: Ui
    , _gameWorld    :: World
    }

-- Lenses
gameCamera   :: Lens' Game Camera
gameKeyAlt   :: Lens' Game Bool
gameKeyShift :: Lens' Game Bool
gamePlayer   :: Lens' Game ObjectId
gameUi       :: Lens' Game Ui
gameWorld    :: Lens' Game World
gameCamera   = lens _gameCamera   (\s x -> s { _gameCamera   = x })
gameKeyAlt   = lens _gameKeyAlt   (\s x -> s { _gameKeyAlt   = x })
gameKeyShift = lens _gameKeyShift (\s x -> s { _gameKeyShift = x })
gamePlayer   = lens _gamePlayer   (\s x -> s { _gamePlayer   = x })
gameUi       = lens _gameUi       (\s x -> s { _gameUi       = x })
gameWorld    = lens _gameWorld    (\s x -> s { _gameWorld    = x })

-- | Default engine state with an empty world, player and camera at 0,0
defaultGame :: Game
defaultGame = MkGame
    { _gameCamera   = defaultCamera
    , _gameKeyAlt   = False
    , _gameKeyShift = False
    , _gamePlayer   = 0
    , _gameUi       = defaultUi
    , _gameWorld    = defaultWorld
    }
