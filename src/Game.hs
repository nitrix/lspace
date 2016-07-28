{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game
    ( Game
    , GameState
    , gameCamera
    , gameKeyAlt
    , gameKeyShift
    , gamePlayer
    , gameShips
    , gameUi
    , gameCreateLink
    , gameModifyLink
    , gameReadLink
    , runGame
    ) where

import Control.Lens
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import qualified Data.Aeson as J

import Camera
import Link
import Object
import Ship
import Ui

-- | Contains the state of the engine (things that will change over time)
data GameState = MkGameState
    { _gameCamera   :: Camera
    , _gameKeyAlt   :: Bool
    , _gameKeyShift :: Bool
    , _gamePlayer   :: Link Object
    , _gameShips    :: [Link (Ship Int Object)]
    , _gameUi       :: Ui
    }

newtype Game a = Game { unwrapGame :: MaybeT (StateT GameState IO) a }
    deriving (Functor, Applicative, Monad, MonadState GameState)

-- Lenses
gameCamera   :: Lens' GameState Camera
gameKeyAlt   :: Lens' GameState Bool
gameKeyShift :: Lens' GameState Bool
gamePlayer   :: Lens' GameState (Link Object)
gameShips    :: Lens' GameState [Link (Ship Int Object)]
gameUi       :: Lens' GameState Ui
gameCamera   = lens _gameCamera   (\s x -> s { _gameCamera   = x })
gameKeyAlt   = lens _gameKeyAlt   (\s x -> s { _gameKeyAlt   = x })
gameKeyShift = lens _gameKeyShift (\s x -> s { _gameKeyShift = x })
gamePlayer   = lens _gamePlayer   (\s x -> s { _gamePlayer   = x })
gameShips    = lens _gameShips    (\s x -> s { _gameShips    = x })
gameUi       = lens _gameUi       (\s x -> s { _gameUi       = x })

instance J.FromJSON GameState where    
    parseJSON (J.Object o) = do
        player <- o J..: "player"
        ships  <- o J..: "ships"
        return $ MkGameState
            { _gameCamera   = defaultCamera
            , _gameKeyAlt   = False
            , _gameKeyShift = False
            , _gamePlayer   = player
            , _gameShips    = ships
            , _gameUi       = defaultUi
            }
    parseJSON _ = error "Unable to parse Game json"

gameReadLink :: J.FromJSON a => Link a -> Game a
gameReadLink = Game . MaybeT . lift . readLink

gameModifyLink :: J.FromJSON a => Link a -> (a -> a) -> Game ()
gameModifyLink link f = Game . MaybeT . lift $ Just <$> modifyLink link f

gameCreateLink :: a -> Game (Link a)
gameCreateLink x = Game . MaybeT . lift $ Just <$> createLink x

runGame :: Game a -> GameState -> IO (Maybe a, GameState)
runGame = runStateT . runMaybeT . unwrapGame