{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Game
    ( Game
    , GameState
    , gameCamera
    , gameKeyAlt
    , gameKeyShift
    , gamePlayer
    , gameShips
    , gameUi
    , runGame
    ) where

import Control.Lens
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.IORef

import Camera
import Types.Cache
import Types.Link
import Types.Object as O
import Types.Ship
import Types.Ui
import Link

-- | Contains the state of the engine (things that will change over time)
data GameState = MkGameState
    { _gameCamera   :: Camera
    , _gameKeyAlt   :: Bool
    , _gameKeyShift :: Bool
    , _gamePlayer   :: Link O.Object
    , _gameShips    :: [Link Ship]
    , _gameUi       :: Ui
    }

newtype Game a = Game { runGame :: MaybeT (StateT GameState IO) a }
    deriving (Functor, Applicative, Monad, MonadState GameState)
    -- TODO remove MonadIO very soon

resolveLink :: FromJSON a => Link a -> Game a
resolveLink link = Game $ MaybeT $ do
    tmpCache <- lift $ newIORef defaultCache -- TODO: caching should be internal to the link module
    lift $ readLink tmpCache link

-- Lenses
gameCamera   :: Lens' GameState Camera
gameKeyAlt   :: Lens' GameState Bool
gameKeyShift :: Lens' GameState Bool
gamePlayer   :: Lens' GameState (Link O.Object)
gameShips    :: Lens' GameState [Link Ship]
gameUi       :: Lens' GameState Ui
gameCamera   = lens _gameCamera   (\s x -> s { _gameCamera   = x })
gameKeyAlt   = lens _gameKeyAlt   (\s x -> s { _gameKeyAlt   = x })
gameKeyShift = lens _gameKeyShift (\s x -> s { _gameKeyShift = x })
gamePlayer   = lens _gamePlayer   (\s x -> s { _gamePlayer   = x })
gameShips    = lens _gameShips    (\s x -> s { _gameShips    = x })
gameUi       = lens _gameUi       (\s x -> s { _gameUi       = x })

instance FromJSON GameState where    
    parseJSON (Object o) = do
        player <- o .: "player"
        ships  <- o .: "ships"
        return $ MkGameState
            { _gameCamera   = defaultCamera
            , _gameKeyAlt   = False
            , _gameKeyShift = False
            , _gamePlayer   = player
            , _gameShips    = ships
            , _gameUi       = defaultUi
            }
    parseJSON _ = error "Unable to parse Game json"
