{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
    ( Game
    , GameState
    , gameCamera
    , gameContext
    , gameEnv
    , gameKeyAlt
    , gameKeyShift
    , gamePlayer
    , gameRegions
    , gameUi
    , gameCreateLink
    , gameDestroyLink
    , gameModifyLink
    , gameWriteLink
    , gameReadLink
    , runGame
    ) where

import Control.Lens hiding (Context)
import Control.Monad.State.Class
import Control.Monad.Except
import Control.Monad.Trans.State hiding (get, gets)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Data.Aeson as J

import Camera
import Environment
import Link
import Object
import Region
import Ui

-- | Contains the state of the engine (things that will change over time)
data GameState = MkGameState
    { _gameCamera   :: Camera
    , _gameContext  :: Context
    , _gameKeyAlt   :: Bool
    , _gameKeyShift :: Bool
    , _gamePlayer   :: Link Object
    , _gameRegions  :: [Link (Region Object)]
    , _gameUi       :: Ui
    }

makeLenses ''GameState

newtype Game a = Game { unwrapGame :: EnvironmentT (MaybeT (StateT GameState IO)) a }
    deriving (Functor, Applicative, Monad, MonadState GameState)

instance J.FromJSON GameState where    
    parseJSON (J.Object o) = do
        player  <- o J..: "player"
        regions <- o J..: "regions"
        return $ MkGameState
            { _gameCamera   = defaultCamera
            , _gameContext  = undefined -- TODO for real?
            , _gameKeyAlt   = False
            , _gameKeyShift = False
            , _gamePlayer   = player
            , _gameRegions  = regions
            , _gameUi       = defaultUi
            }
    parseJSON _ = error "Unable to parse Game json"

instance J.ToJSON GameState where
    toJSON gs = J.object
        [ "player"  J..= _gamePlayer gs
        , "regions" J..= _gameRegions gs
        ]

gameEnv :: (Environment -> a) -> Game a
gameEnv f = Game $ asks f

gameReadLink :: Linkable a => Link a -> Game a
gameReadLink link = do
    ctx <- use gameContext
    Game . lift . MaybeT . lift $ readLink ctx link
    
gameCreateLink :: Linkable a => a -> Game (Link a)
gameCreateLink x = do
    ctx <- use gameContext
    Game . lift . MaybeT . lift $ Just <$> createLink ctx x
    
gameDestroyLink :: Link a -> Game ()
gameDestroyLink link = do
    ctx <- use gameContext
    Game . lift . MaybeT . lift $ Just <$> destroyLink ctx link
    
gameModifyLink :: Linkable a => Link a -> (a -> a) -> Game ()
gameModifyLink link f = do
    ctx <- use gameContext
    Game . lift . MaybeT . lift $ Just <$> modifyLink ctx link f

gameWriteLink :: Linkable a => Link a -> a -> Game ()
gameWriteLink link x = do
    ctx <- use gameContext
    Game . lift . MaybeT . lift $ Just <$> writeLink ctx link x

runGame :: Environment -> GameState -> Game a -> IO (Maybe a, GameState)
runGame env gs game = flip runStateT gs
                    $ runMaybeT
                    $ flip runReaderT env
                    $ unwrapGame game
