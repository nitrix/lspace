{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game
    ( Game
    , GameState
    , gameCamera
    , gameEnv
    , gameKeyAlt
    , gameKeyShift
    , gamePlayer
    , gameShips
    , gameUi
    , gameCreateLink
    , gameModifyLink
    , gameWriteLink
    , gameReadLink
    , runGame
    ) where

import Control.Lens
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Data.Aeson as J

import Camera
import Environment
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

newtype Game a = Game { unwrapGame :: EnvironmentT (MaybeT (StateT GameState IO)) a }
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

instance J.ToJSON GameState where
    toJSON gs = J.object
        [ "player" J..= _gamePlayer gs
        , "ships"  J..= _gameShips gs
        ]

gameEnv :: (Environment -> a) -> Game a
gameEnv f = Game $ asks f

gameReadLink :: Linkable a => Link a -> Game a
gameReadLink link = do
    ctx <- Game $ asks envContext 
    Game . lift . MaybeT . lift $ readLink ctx link
    
gameCreateLink :: a -> Game (Link a)
gameCreateLink x = do
    ctx <- Game $ asks envContext 
    Game . lift . MaybeT . lift $ Just <$> createLink ctx x
    
gameModifyLink :: Linkable a => Link a -> (a -> a) -> Game ()
gameModifyLink link f = do
    ctx <- Game $ asks envContext 
    Game . lift . MaybeT . lift $ Just <$> modifyLink ctx link f

gameWriteLink :: Linkable a => Link a -> a -> Game ()
gameWriteLink link x = do
    ctx <- Game $ asks envContext 
    Game . lift . MaybeT . lift $ Just <$> writeLink ctx link x

runGame :: Environment -> GameState -> Game a -> IO (Maybe a, GameState)
runGame env gs game = flip runStateT gs
                    $ runMaybeT
                    $ flip runReaderT env
                    $ unwrapGame game
