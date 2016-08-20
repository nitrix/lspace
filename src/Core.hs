{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core where

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.State hiding (get, put)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader hiding (ask)

import Environment
import Game

newtype Core a = Core { unwrapCore :: EnvironmentT (MaybeT (StateT GameState IO)) a }
    deriving (Functor, Applicative, Monad, MonadState GameState, MonadReader Environment, MonadIO)

runCore :: Core () -> GameState -> Environment -> IO GameState
runCore core gs env = fmap snd
                    $ flip runStateT gs
                    $ runMaybeT
                    $ flip runReaderT env
                    $ unwrapCore core

embedGame :: Game a -> Core a
embedGame game = Core $ ReaderT $ \env -> MaybeT $ StateT $ \gs -> liftIO $ runGame env gs game
