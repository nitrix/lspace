{-# LANGUAGE OverloadedStrings #-}

module Types.Game
    ( Game
    , gameCamera
    , gameKeyAlt
    , gameKeyShift
    , gamePlayer
    , gameShips
    , gameUi
    ) where

import Control.Lens
import Data.Aeson
import Data.IORef
import System.IO.Unsafe

import Camera
import Types.Cache
import Types.Link
import Types.Object as O
import Types.Ship
import Types.Ui

-- | Contains the state of the engine (things that will change over time)
data Game = MkGame
    { _gameCamera   :: Camera
    , _gameKeyAlt   :: Bool
    , _gameKeyShift :: Bool
    , _gamePlayer   :: Link O.Object
    , _gameShips    :: [Link Ship]
    , _gameUi       :: Ui
    }

-- Lenses
gameCamera   :: Lens' Game Camera
gameKeyAlt   :: Lens' Game Bool
gameKeyShift :: Lens' Game Bool
gamePlayer   :: Lens' Game (Link O.Object)
gameShips    :: Lens' Game [Link Ship]
gameUi       :: Lens' Game Ui
gameCamera   = lens _gameCamera   (\s x -> s { _gameCamera   = x })
gameKeyAlt   = lens _gameKeyAlt   (\s x -> s { _gameKeyAlt   = x })
gameKeyShift = lens _gameKeyShift (\s x -> s { _gameKeyShift = x })
gamePlayer   = lens _gamePlayer   (\s x -> s { _gamePlayer   = x })
gameShips    = lens _gameShips    (\s x -> s { _gameShips    = x })
gameUi       = lens _gameUi       (\s x -> s { _gameUi       = x })

instance FromJSON Game where    
    parseJSON (Object o) = do
        player <- o .: "player"
        ships  <- o .: "ships"
        return $ MkGame
            { _gameCamera   = defaultCamera
            , _gameKeyAlt   = False
            , _gameKeyShift = False
            , _gamePlayer   = player
            , _gameShips    = ships
            , _gameUi       = defaultUi
            }
    parseJSON _ = error "Unable to parse Game json"
